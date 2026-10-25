#lang racket

;; ============================================================
;; transport.rkt — 传输控制
;; 类似 Ardour 的传输面板
;; ============================================================
;;
;; 功能:
;;   - 播放/停止/暂停/录制
;;   - 定位 (前进/后退/跳到标记/回到开头)
;;   - 循环播放
;;   - 录音 (MIDI/音频)
;;   - 磁带风格传输控制
;;

(require "midi-core.rkt"
         "midi-note.rkt"
         "music-stream.rkt"
         "utils/rtmidi.rkt")

(provide transport%
         make-transport)

;; ============================================================
;; transport% — 传输控制器
;; ============================================================
(define transport%
  (class object%
    (super-new)

    (init-field [tempo 120]
                [beats-per-measure 4]
                [beat-subdivision 4])  ; 每拍细分

    (field [state 'stopped]  ; 'stopped, 'playing, 'paused, 'recording
           [position 0.0]   ; 当前位置 (拍)
           [start-time 0.0] ; 播放开始时间 (秒)
           [pause-position 0.0]
           [loop-start 0.0]
           [loop-end 0.0]
           [loop-active? #f]
           [markers (make-hash)] ; 标记点: name → beat
           [streams '()]          ; 关联的 music-stream% 列表
           [device #f]            ; MIDI 输出设备
           [record-track #f]     ; 录音目标轨道
           [record-buffer '()]   ; 录音缓冲
           [record-start-time 0.0]
           [click-track-enabled? #f]
           [tempo-map '()]        ; 速度变化列表: ((beat tempo) ...)
           [time-sig-changes '()] ; 拍号变化: ((beat beats-per-measure beat-subdivision) ...)
           [metronome-thread #f]
           [play-thread #f])

    ;; ---- 状态查询 ----

    (define/public (get-state) state)
    (define/public (is-playing?) (eq? state 'playing))
    (define/public (is-paused?) (eq? state 'paused))
    (define/public (is-stopped?) (eq? state 'stopped))
    (define/public (is-recording?) (eq? state 'recording))
    (define/public (get-position) position)

    ;; ---- 播放控制 ----

    ;; 播放
    (define/public (play [dev #f])
      (when dev (set! device dev))
      (unless device
        (error 'transport "未设置 MIDI 设备"))
      (case state
        [(stopped)
         (set! position 0.0)
         (set! start-time (current-inexact-milliseconds))
         (set! state 'playing)
         (start-play-thread)
         (when click-track-enabled?
           (start-click-track))]
        [(paused)
         (set! start-time (- (current-inexact-milliseconds)
                            (* pause-position (/ 60.0 tempo) 1000.0)))
         (set! state 'playing)
         (start-play-thread)])
      this)

    ;; 停止
    (define/public (stop)
      (set! state 'stopped)
      (set! position 0.0)
      (stop-click-track)
      (stop-play-thread)
      ;; 发送所有音符关闭
      (when device
        (for ([ch (in-range 16)])
          (send device all-notes-off ch)))
      this)

    ;; 暂停
    (define/public (pause)
      (when (eq? state 'playing)
        (set! pause-position position)
        (set! state 'paused)
        (stop-click-track)
        (stop-play-thread)
        ;; 发送所有音符关闭
        (when device
          (for ([ch (in-range 16)])
            (send device all-notes-off ch))))
      this)

    ;; 录制
    (define/public (record [dev #f])
      (when dev (set! device dev))
      (set! position 0.0)
      (set! start-time (current-inexact-milliseconds))
      (set! record-buffer '())
      (set! record-start-time (current-inexact-milliseconds))
      (set! state 'recording)
      (start-play-thread)
      this)

    ;; ---- 定位 ----

    ;; 跳到开头
    (define/public (go-to-start)
      (if (eq? state 'playing)
          (begin
            (set! position 0.0)
            (set! start-time (current-inexact-milliseconds)))
          (set! position 0.0))
      this)

    ;; 跳到结尾
    (define/public (go-to-end)
      (let ([end (if loop-active? loop-end (estimate-end))])
        (if (eq? state 'playing)
            (begin
              (set! position end)
              (set! start-time (- (current-inexact-milliseconds)
                                 (* end (/ 60.0 tempo) 1000.0))))
            (set! position end)))
      this)

    ;; 前进 N 拍
    (define/public (forward beats)
      (set! position (+ position beats))
      (when (and loop-active? (>= position loop-end))
        (set! position loop-start))
      this)

    ;; 后退 N 拍
    (define/public (rewind beats)
      (set! position (max 0.0 (- position beats)))
      this)

    ;; 跳到标记
    (define/public (go-to-marker name)
      (when (hash-has-key? markers name)
        (let ([pos (hash-ref markers name)])
          (if (eq? state 'playing)
              (begin
                (set! position pos)
                (set! start-time (- (current-inexact-milliseconds)
                                   (* pos (/ 60.0 tempo) 1000.0))))
              (set! position pos))))
      this)

    ;; ---- 标记 ----

    (define/public (set-marker! name beat)
      (hash-set! markers name beat)
      this)

    (define/public (get-marker name)
      (hash-ref markers name #f))

    (define/public (get-markers) markers)

    (define/public (remove-marker! name)
      (hash-remove! markers name)
      this)

    ;; ---- 循环 ----

    (define/public (set-loop-point! start end)
      (set! loop-start start)
      (set! loop-end end)
      this)

    (define/public (enable-loop!) (set! loop-active? #t) this)
    (define/public (disable-loop!) (set! loop-active? #f) this)
    (define/public (is-loop-active?) loop-active?)

    ;; ---- 速度 ----

    (define/public (get-tempo) tempo)
    (define/public (set-tempo! t)
      (set! tempo (max 1.0 (min 300.0 t)))
      this)

    ;; 速度变化 (在指定拍)
    (define/public (add-tempo-change! beat new-tempo)
      (set! tempo-map (cons (list beat new-tempo)
                            (sort tempo-map < #:key car)))
      this)

    (define/public (get-tempo-at beat)
      (let ([changes (filter (lambda (tc) (<= (car tc) beat)) tempo-map)])
        (if (null? changes) tempo (cadr (last changes)))))

    ;; ---- 拍号 ----

    (define/public (set-time-signature! beats measure)
      (set! beats-per-measure beats)
      (set! beat-subdivision measure)
      this)

    (define/public (add-time-sig-change! beat beats measure)
      (set! time-sig-changes
            (cons (list beat beats measure)
                  (sort time-sig-changes < #:key car)))
      this)

    ;; ---- 流管理 ----

    (define/public (add-stream! stream)
      (unless (memq stream streams)
        (set! streams (append streams (list stream))))
      this)

    (define/public (remove-stream! stream)
      (set! streams (filter (lambda (s) (not (eq? s stream))) streams))
      this)

    ;; ---- 点击轨 ----

    (define/public (enable-click-track!)
      (set! click-track-enabled? #t)
      this)
    (define/public (disable-click-track!)
      (set! click-track-enabled? #f)
      (stop-click-track)
      this)
    (define/public (is-click-track-enabled?) click-track-enabled?)

    ;; ---- 录音 ----

    (define/public (set-record-track! track)
      (set! record-track track)
      this)

    (define/public (get-record-buffer) record-buffer)

    (define/public (add-to-record-buffer! event)
      (let* ([elapsed (- (current-inexact-milliseconds) record-start-time)]
             [beat (* elapsed (/ tempo 60000.0))])
        (set! record-buffer (append record-buffer (list (cons beat event)))))
      this)

    (define/public (clear-record-buffer!)
      (set! record-buffer '())
      this)

    ;; ---- 时间转换 ----

    (define/public (beat->sec beat)
      (* beat (/ 60.0 (get-tempo-at beat))))

    (define/public (sec->beat sec)
      (* sec (/ (get-tempo-at 0) 60.0)))

    ;; ---- 内部方法 ----

    (define (start-play-thread)
      (stop-play-thread)
      (set! play-thread
            (thread
             (lambda ()
               (let loop ()
                 (when (or (eq? state 'playing) (eq? state 'recording))
                   (update-position!)
                   ;; 检查循环
                   (when (and loop-active? (>= position loop-end))
                     (set! position loop-start)
                     (set! start-time (current-inexact-milliseconds))
                     (for ([s streams])
                       (send s stop)))
                   (sleep 0.001)
                   (loop)))))))

    (define (stop-play-thread)
      (when play-thread
        (kill-thread play-thread)
        (set! play-thread #f)))

    (define (update-position!)
      (let* ([elapsed (- (current-inexact-milliseconds) start-time)]
             [sec (/ elapsed 1000.0)]
             [beats (* sec (/ tempo 60.0))])
        (set! position beats)))

    (define (estimate-end)
      (if (null? streams) 32.0
          (apply max (map (lambda (s) (send s beats-remaining)) streams))))

    (define (start-click-track)
      (stop-click-track)
      (set! metronome-thread
            (thread
             (lambda ()
               (let loop ()
                 (when (or (eq? state 'playing) (eq? state 'recording))
                   (let ([beat-in-measure (modulo (inexact->exact (floor position))
                                                  beats-per-measure)])
                     ;; 强拍用高音, 弱拍用低音
                     (when device
                       (if (= beat-in-measure 0)
                           (send device note-on 9 76 100)  ; 高木鱼
                           (send device note-on 9 77 80))) ; 低木鱼
                     (sleep (beat->sec 1.0))
                     (when device
                       (send device note-off 9 (if (= beat-in-measure 0) 76 77)))))
                 (loop))))))

    (define (stop-click-track)
      (when metronome-thread
        (kill-thread metronome-thread)
        (set! metronome-thread #f)))
    ))

;; ============================================================
;; 便捷构造
;; ============================================================

(define (make-transport [tempo 120] [beats-per-measure 4])
  (new transport% [tempo tempo] [beats-per-measure beats-per-measure]))
