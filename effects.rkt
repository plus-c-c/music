#lang racket

;; ============================================================
;; effects.rkt — 音频效果处理
;; 类似 Ardour 的内置效果器
;; ============================================================
;;
;; 功能:
;;   - 混响 (Reverb)
;;   - 延迟 (Delay)
;;   - 均衡器 (EQ)
;;   - 压缩器 (Compressor)
;;   - 限制器 (Limiter)
;;   - 增益 (Gain)
;;   - 声像 (Pan)
;;   - 噪声门 (Noise Gate)
;;   - 失真 (Distortion)
;;   - 合唱 (Chorus)
;;

(provide effect%
         reverb%
         delay%
         eq%
         compressor%
         limiter%
         gain%
         pan-effect%
         gate%
         distortion%
         chorus%
         make-effect-chain
         process-midi-effects)

;; ============================================================
;; effect% — 效果器基类
;; ============================================================
(define effect%
  (class object%
    (super-new)

    (init-field [name "Effect"]
                [enabled? #t]
                [wet/dry 1.0])  ; 0.0=纯干 1.0=纯湿

    (define/public (process-event event)
      ;; 子类重写此方法
      event)

    (define/public (process-events events)
      (if enabled?
          (map (lambda (e) (process-event e)) events)
          events))

    (define/public (enable!) (set! enabled? #t) this)
    (define/public (disable!) (set! enabled? #f) this)
    (define/public (is-enabled?) enabled?)
    (define/public (set-wet/dry! wd) (set! wet/dry (max 0.0 (min 1.0 wd))) this)
    (define/public (get-wet/dry) wet/dry)
    (define/public (get-name) name)

    ;; 混合干湿信号
    (define (mix-wet-dry dry-event wet-event)
      (let ([dry-level (- 1.0 wet/dry)]
            [wet-level wet/dry])
        (if (and (list? dry-event) (list? wet-event) (= (length dry-event) 3))
            ;; MIDI 事件: 混合力度
            (let ([dry-vel (caddr dry-event)]
                  [wet-vel (caddr wet-event)])
              (list (car dry-event) (cadr dry-event)
                    (inexact->exact (round (+ (* dry-vel dry-level)
                                             (* wet-vel wet-level))))))
            wet-event)))
    ))

;; ============================================================
;; reverb% — 混响效果器
;; ============================================================
(define reverb%
  (class effect%
    (super-new)

    (init-field [room-size 0.5]    ; 0.0 ~ 1.0 房间大小
                [damping 0.5]      ; 0.0 ~ 1.0 阻尼
                [pre-delay 20.0])  ; 预延迟 (ms)

    (field [delay-line (make-vector 1024 0)]
           [write-pos 0])

    (define/public (get-room-size) room-size)
    (define/public (set-room-size! rs) (set! room-size (max 0.0 (min 1.0 rs))) this)
    (define/public (get-damping) damping)
    (define/public (set-damping! d) (set! damping (max 0.0 (min 1.0 d))) this)
    (define/public (get-pre-delay) pre-delay)
    (define/public (set-pre-delay! pd) (set! pre-delay (max 0.0 pd)) this)

    (define/override (process-event event)
      (if (and (list? event) (= (length event) 3)
               (= (bitwise-and (car event) #xF0) #x90))  ; Note On
          (let* ([vel (caddr event)]
                 [feedback (* room-size 0.9)]
                 [damp-factor (- 1.0 (* damping 0.5))]
                 [pre-delay-samples (inexact->exact (round (* pre-delay 44.1)))]
                 [delay-idx (modulo (- write-pos pre-delay-samples) (vector-length delay-line))]
                 [echo-vel (inexact->exact
                            (round (* vel (* (get-field wet/dry this) feedback))) )])
            ;; 写入延迟线
            (vector-set! delay-line write-pos vel)
            (set! write-pos (modulo (+ write-pos 1) (vector-length delay-line)))
            ;; 输出: 原始 + 混响尾音
            (if (> echo-vel 1)
                (list (car event) (cadr event)
                      (min 127 (+ vel (inexact->exact (round (* echo-vel 0.3))))))
                event))
          event))
    ))

;; ============================================================
;; delay% — 延迟效果器
;; ============================================================
(define delay%
  (class effect%
    (super-new)

    (init-field [time 0.5]          ; 延迟时间 (秒)
                [feedback 0.4]      ; 反馈量 0.0 ~ 0.95
                [filter-freq 5000]  ; 滤波频率 (Hz)
                [sync-to-tempo? #f] ; 同步到节拍
                [tempo 120]
                [delay-note 'quarter]) ; 同步音符: 'quarter, 'eighth, 'dotted-eighth, etc.

    (field [buffer '()]
           [last-output-time 0])

    (define/public (get-time) time)
    (define/public (set-time! t) (set! time (max 0.01 t)) this)
    (define/public (get-feedback) feedback)
    (define/public (set-feedback! f) (set! feedback (max 0.0 (min 0.95 f))) this)

    ;; 根据拍号计算同步延迟时间
    (define (compute-sync-time)
      (let* ([beat-duration (/ 60.0 tempo)]
             [multiplier (case delay-note
                           [(quarter) 1.0]
                           [(eighth) 0.5]
                           [(sixteenth) 0.25]
                           [(dotted-eighth) 1.5]
                           [(dotted-quarter) 0.75]
                           [(half) 2.0]
                           [(whole) 4.0]
                           [else 1.0])])
        (* beat-duration multiplier)))

    (define/override (process-event event)
      (if (and (list? event) (= (length event) 3)
               (= (bitwise-and (car event) #xF0) #x90))
          (let* ([vel (caddr event)]
                 [delay-vel (inexact->exact (round (* vel feedback)))])
            (when (> delay-vel 1)
              ;; 添加延迟副本到 buffer
              (let ([delay-event (list (car event) (cadr event) (min 127 delay-vel))])
                (set! buffer (append buffer (list (cons (+ last-output-time
                                                          (if sync-to-tempo?
                                                              (compute-sync-time)
                                                              time))
                                                      delay-event))))))
            (set! last-output-time (+ last-output-time 0.001)) ; 简化: 使用绝对时间
            event)
          event))

    (define/override (process-events events)
      (if (get-field enabled? this)
          (let ([processed (map (lambda (e) (process-event e)) events)])
            ;; 合并缓冲区中的延迟事件
            (sort (append processed (map cdr buffer))
                  <
                  #:key (lambda (e) (if (list? e) (car e) 0))))
          events))
    ))

;; ============================================================
;; eq% — 均衡器
;; ============================================================
(define eq%
  (class effect%
    (super-new)

    ;; 参数化 3 段 EQ
    (init-field [low-freq 200]      ; 低频截止 (Hz)
                [low-gain 0.0]      ; 低频增益 (dB)
                [mid-freq 1000]     ; 中频中心 (Hz)
                [mid-gain 0.0]      ; 中频增益 (dB)
                [mid-q 1.0]         ; 中频 Q 值
                [high-freq 4000]    ; 高频截止 (Hz)
                [high-gain 0.0])    ; 高频增益 (dB)

    (define/public (set-low-gain! g) (set! low-gain (max -24.0 (min 24.0 g))) this)
    (define/public (set-mid-gain! g) (set! mid-gain (max -24.0 (min 24.0 g))) this)
    (define/public (set-high-gain! g) (set! high-gain (max -24.0 (min 24.0 g))) this)
    (define/public (set-low-freq! f) (set! low-freq (max 20.0 (min 500.0 f))) this)
    (define/public (set-mid-freq! f) (set! mid-freq (max 100.0 (min 8000.0 f))) this)
    (define/public (set-high-freq! f) (set! high-freq (max 1000.0 (min 20000.0 f))) this)
    (define/public (set-mid-q! q) (set! mid-q (max 0.1 (min 10.0 q))) this)

    ;; 对 MIDI 力度应用 EQ 增益 (简化: 基于频率映射到力度)
    (define/override (process-event event)
      (if (and (list? event) (= (length event) 3))
          (let* ([vel (caddr event)]
                 ;; 简化: 将 MIDI 音高映射到频段
                 ;; 低音 (C2-B2) → low-gain
                 ;; 中音 (C3-B4) → mid-gain
                 ;; 高音 (C5+) → high-gain
                 [pitch (cadr event)]
                 [freq (* 440.0 (expt 2.0 (/ (- pitch 69) 12.0)))]
                 [gain-db (cond
                            [(< freq low-freq) low-gain]
                            [(< freq mid-freq) mid-gain]
                            [else high-gain])]
                 ;; dB → 线性
                 [gain-linear (expt 10.0 (/ gain-db 20.0))]
                 [new-vel (inexact->exact (round (* vel gain-linear)))])
            (list (car event) (cadr event) (max 0 (min 127 new-vel))))
          event))
    ))

;; ============================================================
;; compressor% — 压缩器
;; ============================================================
(define compressor%
  (class effect%
    (super-new)

    (init-field [threshold -20.0]  ; 阈值 (dB)
                [ratio 4.0]        ; 压缩比 (1:n)
                [attack 10.0]      ; 启动时间 (ms)
                [release 100.0]    ; 释放时间 (ms)
                [knee 6.0])        ; 拐点宽度 (dB)

    (field [current-gain 0.0]
           [gain-history '()])

    (define/public (set-threshold! t) (set! threshold (max -60.0 (min 0.0 t))) this)
    (define/public (set-ratio! r) (set! ratio (max 1.0 (min 30.0 r))) this)
    (define/public (set-attack! a) (set! attack (max 0.1 (min 100.0 a))) this)
    (define/public (set-release! r) (set! release (max 10.0 (min 2000.0 r))) this)

    (define/override (process-event event)
      (if (and (list? event) (= (length event) 3)
               (= (bitwise-and (car event) #xF0) #x90))
          (let* ([vel (caddr event)]
                 [vel-db (if (> vel 0) (* 20.0 (log (/ vel 127.0) 10.0)) -60.0)]
                 [over-db (max 0.0 (- vel-db threshold))]
                 [compressed-db (if (> over-db knee)
                                    (* over-db (- 1.0 (/ 1.0 ratio)))
                                    (* over-db (- 1.0 (/ 1.0 ratio)) 0.5))]
                 [gain-db (- compressed-db)]
                 [gain-linear (expt 10.0 (/ gain-db 20.0))]
                 [new-vel (inexact->exact (round (* vel gain-linear)))])
            (set! current-gain gain-db)
            (list (car event) (cadr event) (max 0 (min 127 new-vel))))
          event))

    (define/public (get-current-gain) current-gain)
    (define/public (get-reduction) (- current-gain))
    ))

;; ============================================================
;; limiter% — 限制器
;; ============================================================
(define limiter%
  (class effect%
    (super-new)

    (init-field [ceiling -0.1]      ; 天花板 (dB)
                [release 50.0])     ; 释放时间 (ms)

    (define/override (process-event event)
      (if (and (list? event) (= (length event) 3))
          (let* ([vel (caddr event)]
                 [ceiling-linear (* 127.0 (expt 10.0 (/ ceiling 20.0)))]
                 [new-vel (min (inexact->exact (round ceiling-linear)) vel)])
            (list (car event) (cadr event) (max 0 (min 127 new-vel))))
          event))
    ))

;; ============================================================
;; gain% — 增益效果器
;; ============================================================
(define gain%
  (class effect%
    (super-new)

    (init-field [gain 0.0])  ; 增益 (dB)

    (define/public (get-gain) gain)
    (define/public (set-gain! g) (set! gain (max -48.0 (min 48.0 g))) this)

    (define/override (process-event event)
      (if (and (list? event) (= (length event) 3))
          (let* ([vel (caddr event)]
                 [gain-linear (expt 10.0 (/ gain 20.0))]
                 [new-vel (inexact->exact (round (* vel gain-linear)))])
            (list (car event) (cadr event) (max 0 (min 127 new-vel))))
          event))
    ))

;; ============================================================
;; pan-effect% — 声像效果器
;; ============================================================
(define pan-effect%
  (class effect%
    (super-new)

    (init-field [pan 0.0])  ; -1.0 (左) ~ 1.0 (右)

    (define/public (get-pan) pan)
    (define/public (set-pan! p) (set! pan (max -1.0 (min 1.0 p))) this)

    ;; 对 MIDI 消息应用声像 (CC10)
    (define/override (process-event event)
      (if (and (list? event) (= (length event) 3))
          (let* ([cc-value (inexact->exact (round (* (+ pan 1.0) 63.5)))]
                 ;; 输出声像 CC 消息
                 [pan-msg (list #xB0 10 cc-value)])  ; CC10 = 声像
            ;; 原始事件 + 声像 CC
            event)  ; 声像由混音器处理, 此处仅记录
          event))
    ))

;; ============================================================
;; gate% — 噪声门
;; ============================================================
(define gate%
  (class effect%
    (super-new)

    (init-field [threshold 20]     ; 阈值 (velocity)
                [attack 5]         ; 启动时间 (ms)
                [hold 50]          ; 保持时间 (ms)
                [release 100])     ; 释放时间 (ms)

    (field [open? #f]
           [hold-timer 0])

    (define/public (set-threshold! t) (set! threshold (max 0 (min 127 t))) this)

    (define/override (process-event event)
      (if (and (list? event) (= (length event) 3))
          (let ([vel (caddr event)])
            (cond
              [(>= vel threshold)
               (set! open? #t)
               (set! hold-timer (+ (current-inexact-milliseconds) hold))
               event]
              [(and open? (< (current-inexact-milliseconds) hold-timer))
               event]  ; 保持打开
              [else
               (set! open? #f)
               ;; 静音
               (list (car event) (cadr event) 0)]))
          event))
    ))

;; ============================================================
;; distortion% — 失真效果器
;; ============================================================
(define distortion%
  (class effect%
    (super-new)

    (init-field [drive 0.5]        ; 驱动量 0.0 ~ 1.0
                [tone 0.5])        ; 音色 0.0 ~ 1.0

    (define/public (set-drive! d) (set! drive (max 0.0 (min 1.0 d))) this)
    (define/public (set-tone! t) (set! tone (max 0.0 (min 1.0 t))) this)

    (define/override (process-event event)
      (if (and (list? event) (= (length event) 3))
          (let* ([vel (caddr event)]
                 ;; soft clipping
                 [normalized (/ vel 127.0)]
                 [driven (* normalized (+ 1.0 (* drive 10.0)))]
                 ;; tanh soft clip
                 [clipped (tanh driven)]
                 [new-vel (inexact->exact (round (* 127.0 (abs clipped))))])
            (list (car event) (cadr event) (max 0 (min 127 new-vel))))
          event))
    ))

;; ============================================================
;; chorus% — 合唱效果器
;; ============================================================
(define chorus%
  (class effect%
    (super-new)

    (init-field [rate 1.5]          ; 调制速率 (Hz)
                [depth 0.5]         ; 调制深度 0.0 ~ 1.0
                [mix 0.5])          ; 混合量 0.0 ~ 1.0

    (field [phase 0.0])

    (define/public (set-rate! r) (set! rate (max 0.1 (min 10.0 r))) this)
    (define/public (set-depth! d) (set! depth (max 0.0 (min 1.0 d))) this)
    (define/public (set-mix! m) (set! mix (max 0.0 (min 1.0 m))) this)

    (define/override (process-event event)
      (if (and (list? event) (= (length event) 3)
               (= (bitwise-and (car event) #xF0) #x90))
          (let* ([vel (caddr event)]
                 ;; 调制延迟
                 [mod (* depth (sin phase))]
                 [delay-ms (* mod 10.0)]
                 ;; 微调力度模拟合唱
                 [chorus-vel (inexact->exact
                              (round (+ vel (* vel 0.1 (sin phase)))))]
                 ;; 混合
                 [mixed-vel (inexact->exact
                             (round (+ (* vel (- 1.0 mix))
                                       (* chorus-vel mix))))])
            (set! phase (+ phase (* 2.0 pi rate 0.001)))
            (list (car event) (cadr event) (max 0 (min 127 mixed-vel))))
          event))
    ))

;; ============================================================
;; 效果链管理
;; ============================================================

(define (make-effect-chain . effects)
  effects)

(define (process-midi-effects events effects)
  (for/fold ([result events])
            ([fx effects])
    (send fx process-events result))
  )
