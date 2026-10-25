#lang racket

;; ============================================================
;; metronome.rkt — 节拍器/点击轨
;; ============================================================
;;
;; 功能:
;;   - 独立节拍器 (可选)
;;   - 点击音色配置 (强拍/弱拍/次强拍)
;;   - 节拍预热 (pre-count)
;;   - 通过 MIDI 设备输出
;;

(require "midi-core.rkt")

(provide metronome%
         make-metronome)

(define metronome%
  (class object%
    (super-new)

    (init-field [tempo 120]
                [beats-per-measure 4]
                [beat-subdivision 4])

    (field [enabled? #t]
           [volume 100]
           ;; 点击音色 (MIDI drum numbers)
           [accent-sound 76]   ; 高木鱼 (强拍)
           [normal-sound 77]   ; 低木鱼 (弱拍)
           [offbeat-sound 77]  ; 低木鱼 (弱拍细分)
           [pre-count? #f]     ; 预热
           [pre-count-bars 1]  ; 预热小节数
           [channel 9]         ; 打击乐通道
           [device #f]
           [running? #f]
           [click-thread #f])

    (define/public (enable!) (set! enabled? #t) this)
    (define/public (disable!) (set! enabled? #f) this)
    (define/public (is-enabled?) enabled?)

    (define/public (get-tempo) tempo)
    (define/public (set-tempo! t) (set! tempo (max 1.0 (min 300.0 t))) this)

    (define/public (set-time-signature! beats subdivision)
      (set! beats-per-measure beats)
      (set! beat-subdivision subdivision)
      this)

    (define/public (set-volume! v) (set! volume (max 0 (min 127 v))) this)
    (define/public (get-volume) volume)

    (define/public (set-sounds! accent normal [offbeat #f])
      (set! accent-sound accent)
      (set! normal-sound normal)
      (when offbeat (set! offbeat-sound offbeat))
      this)

    ;; 预热
    (define/public (enable-pre-count! [bars 1])
      (set! pre-count? #t)
      (set! pre-count-bars bars)
      this)
    (define/public (disable-pre-count!)
      (set! pre-count? #f)
      this)

    ;; 启动节拍器
    (define/public (start dev)
      (set! device dev)
      (set! running? #t)
      (when pre-count?
        (play-pre-count))
      (set! click-thread (thread (λ () (play-loop))))
      this)

    (define/public (stop)
      (set! running? #f)
      (when click-thread
        (kill-thread click-thread)
        (set! click-thread #f))
      this)

    ;; 播放单个点击
    (define/public (click [beat 0])
      (when (and device enabled?)
        (let* ([beat-in-measure (modulo beat beats-per-measure)]
               [sound (cond
                        [(= beat-in-measure 0) accent-sound]
                        [(= beat-in-measure (/ beats-per-measure 2)) normal-sound]
                        [else offbeat-sound])])
          (send device note-on channel sound volume)
          (thread
           (lambda ()
             (sleep 0.05)
             (send device note-off channel sound))))))

    ;; 播放预热
    (define/private (play-pre-count)
      (when device
        (for* ([bar (in-range pre-count-bars)]
               [beat (in-range beats-per-measure)])
          (let ([sound (if (= beat 0) accent-sound normal-sound)])
            (send device note-on channel sound (quotient volume 2))
            (sleep (beat->sec 1.0))
            (send device note-off channel sound)))))

    ;; 内部播放循环
    (define/private (play-loop)
      (let loop ([beat 0])
        (when running?
          (click beat)
          (sleep (beat->sec 1.0))
          (loop (modulo (+ beat 1) beats-per-measure)))))

    ;; 拍→秒
    (define/private (beat->sec beats)
      (* beats (/ 60.0 tempo)))
    ))

(define (make-metronome [tempo 120] [beats 4])
  (new metronome% [tempo tempo] [beats-per-measure beats]))
