#lang racket

;; ============================================================
;; mixer.rkt — 多轨混音器
;; 类似 Ardour 的混音台功能
;; ============================================================
;;
;; 功能:
;;   - 音轨管理 (音量/声像/静音/独奏/增益/相位反转)
;;   - 总线路由 (音轨 → 总线 → 主输出)
;;   - VCA 分组 (控制多个轨道的音量)
;;   - 信号链 (轨道 → 效果链 → 总线)
;;   - MIDI CC 映射 (CC7=音量, CC10=声像, CC11=表情)
;;

(require "midi-core.rkt"
         "midi-note.rkt"
         "midi-dynamics.rkt")

(provide mixer%
         mixer-track%
         mixer-bus%
         mixer-vca%
         make-mixer
         make-track
         make-bus)

;; ============================================================
;; mixer-track% — 单个音轨
;; ============================================================
(define mixer-track%
  (class object%
    (super-new)

    (init-field [name "Track"]
                [channel 0]
                [volume 1.0]       ; 0.0 ~ 2.0 (1.0 = 0dB)
                [pan 0.0]          ; -1.0 (左) ~ 1.0 (右)
                [mute? #f]
                [solo? #f]
                [phase-invert? #f]
                [gain 1.0]         ; 前置增益 (0.0 ~ 10.0)
                [color #f])        ; 标签颜色

    (field [effects '()]           ; 效果链 (effect% 对象列表)
           [volume-automation #f]  ; 音量自动化
           [pan-automation #f])    ; 声像自动化

    ;; ---- 音量控制 ----

    (define/public (get-volume) volume)
    (define/public (set-volume! v)
      (set! volume (clamp v 0.0 2.0))
      this)
    (define/public (set-volume-db! db)
      ;; dB → linear: 10^(db/20)
      (set! volume (expt 10.0 (/ db 20.0)))
      this)
    (define/public (get-volume-db)
      ;; linear → dB: 20*log10(v)
      (if (> volume 0.0)
          (* 20.0 (log volume 10.0))
          -inf.0))

    ;; ---- 声像控制 ----

    (define/public (get-pan) pan)
    (define/public (set-pan! p)
      (set! pan (clamp p -1.0 1.0))
      this)

    ;; ---- 静音/独奏 ----

    (define/public (is-muted?) mute?)
    (define/public (set-mute! m)
      (set! mute? m)
      this)
    (define/public (is-solo?) solo?)
    (define/public (set-solo! s)
      (set! solo? s)
      this)

    ;; ---- 增益 ----

    (define/public (get-gain) gain)
    (define/public (set-gain! g)
      (set! gain (clamp g 0.0 10.0))
      this)

    ;; ---- 相位 ----

    (define/public (is-phase-inverted?) phase-invert?)
    (define/public (set-phase-invert! p)
      (set! phase-invert? p)
      this)

    ;; ---- 效果链 ----

    (define/public (add-effect! effect)
      (set! effects (append effects (list effect)))
      this)
    (define/public (remove-effect! effect)
      (set! effects (filter (lambda (e) (not (eq? e effect))) effects))
      this)
    (define/public (get-effects) effects)
    (define/public (clear-effects!)
      (set! effects '())
      this)

    ;; ---- 计算最终增益 ----
    ;; 考虑 mute/solo/音量/增益/相位
    (define/public (compute-gain solo-active?)
      (cond
        [mute? 0.0]
        [(and solo-active? (not solo?)) 0.0]
        [else
         (let* ([g (* gain volume)]
                [g (if phase-invert? (- g) g)])
           ;; 声像衰减: 等功率 panning
           g)]))

    ;; ---- 计算声像左右增益 ----
    ;; 等功率 panning: L = cos(π/4 * (1+pan)), R = sin(π/4 * (1+pan))
    (define/public (compute-pan-gains)
      (let* ([angle (* (/ pi 4.0) (+ 1.0 pan))]
             [l (* (cos angle) (compute-gain #f))]
             [r (* (sin angle) (compute-gain #f))])
        (values l r)))

    ;; ---- MIDI CC 映射 ----

    ;; CC7 = 音量, CC10 = 声像, CC11 = 表情
    (define/public (apply-cc cc value)
      (let ([norm (/ value 127.0)])
        (case cc
          [(7)  (set-volume! (* 2.0 norm))]    ; CC7 → 0~200%
          [(10) (set! pan (- (* 2.0 norm) 1.0))] ; CC10 → -1~1
          [(11) (void)] ; CC11 表情 → 由效果链处理
          [else (void)]))
      this)

    ;; ---- 获取 MIDI 消息 ----

    (define/public (note-on note [velocity 100])
      (let ([vel (inexact->exact (round (* velocity (compute-gain #f))))])
        (list (+ NOTE-ON channel) note (clamp vel 0 127))))

    (define/public (note-off note)
      (list (+ NOTE-OFF channel) note 0))

    (define/public (program-change program)
      (list (+ PROGRAM-CHANGE channel) program))

    (define/public (control-change cc value)
      (list (+ CONTROL-CHANGE channel) cc (clamp value 0 127)))

    ;; ---- 辅助 ----

    (define (clamp v lo hi)
      (max lo (min hi v)))
    ))

;; ============================================================
;; mixer-bus% — 总线
;; ============================================================
(define mixer-bus%
  (class object%
    (super-new)

    (init-field [name "Bus"]
                [type 'submix])  ; 'submix, 'master, 'vca

    (field [volume 1.0]
           [pan 0.0]
           [mute? #f]
           [solo? #f]
           [tracks '()]        ; 路由到此总线的音轨
           [send-levels (make-hash)]) ; 发送电平: track-id → level

    ;; ---- 音量 ----

    (define/public (get-volume) volume)
    (define/public (set-volume! v)
      (set! volume (max 0.0 (min 2.0 v)))
      this)

    ;; ---- 声像 ----

    (define/public (get-pan) pan)
    (define/public (set-pan! p)
      (set! pan (max -1.0 (min 1.0 p)))
      this)

    ;; ---- 静音/独奏 ----

    (define/public (is-muted?) mute?)
    (define/public (set-mute! m) (set! mute? m) this)
    (define/public (is-solo?) solo?)
    (define/public (set-solo! s) (set! solo? s) this)

    ;; ---- 路由 ----

    (define/public (add-track! track)
      (unless (memq track tracks)
        (set! tracks (append tracks (list track))))
      this)

    (define/public (remove-track! track)
      (set! tracks (filter (lambda (t) (not (eq? t track))) tracks))
      this)

    (define/public (get-tracks) tracks)

    ;; ---- 发送 ----

    (define/public (set-send-level! track level)
      (hash-set! send-levels track (max 0.0 (min 2.0 level)))
      this)

    (define/public (get-send-level track)
      (hash-ref send-levels track 0.0))
    ))

;; ============================================================
;; mixer-vca% — VCA 分组
;; ============================================================
(define mixer-vca%
  (class object%
    (super-new)

    (init-field [name "VCA"]
                [level 1.0])     ; 0.0 ~ 2.0

    (field [tracks '()])

    (define/public (get-level) level)
    (define/public (set-level! l)
      (set! level (max 0.0 (min 2.0 l)))
      this)

    (define/public (add-track! track)
      (unless (memq track tracks)
        (set! tracks (append tracks (list track))))
      this)

    (define/public (remove-track! track)
      (set! tracks (filter (lambda (t) (not (eq? t track))) tracks))
      this)

    (define/public (get-tracks) tracks)

    ;; 应用 VCA 电平到所有关联轨道
    (define/public (apply!)
      (for-each (lambda (t)
                  (send t set-volume! (* (send t get-volume) level)))
                tracks)
      this)
    ))

;; ============================================================
;; mixer% — 混音台
;; ============================================================
(define mixer%
  (class object%
    (super-new)

    (field [tracks '()]     ; 所有音轨
           [buses '()]      ; 所有总线
           [vcas '()]       ; 所有 VCA
           [master-bus #f]  ; 主总线
           [tempo 120]
           [beats-per-measure 4])

    ;; 初始化主总线
    (set! master-bus (new mixer-bus% [name "Master"] [type 'master]))

    ;; ---- 创建音轨 ----

    (define/public (add-track! track)
      (set! tracks (append tracks (list track)))
      (send master-bus add-track! track)
      track)

    (define/public (create-track! name [channel 0] [volume 1.0] [pan 0.0])
      (let ([t (new mixer-track% [name name] [channel channel]
                    [volume volume] [pan pan])])
        (add-track! t)
        t))

    (define/public (remove-track! track)
      (set! tracks (filter (lambda (t) (not (eq? t track))) tracks))
      (send master-bus remove-track! track)
      this)

    (define/public (get-track name)
      (findf (lambda (t) (string=? (get-field name t) name)) tracks))

    (define/public (get-track-at idx)
      (if (< idx (length tracks))
          (list-ref tracks idx)
          #f))

    ;; ---- 创建总线 ----

    (define/public (add-bus! bus)
      (set! buses (append buses (list bus)))
      bus)

    (define/public (create-bus! name [type 'submix])
      (let ([b (new mixer-bus% [name name] [type type])])
        (add-bus! b)
        b))

    (define/public (remove-bus! bus)
      (set! buses (filter (lambda (b) (not (eq? b bus))) buses))
      this)

    (define/public (get-bus name)
      (findf (lambda (b) (string=? (get-field name b) name)) buses))

    ;; ---- 创建 VCA ----

    (define/public (add-vca! vca)
      (set! vcas (append vcas (list vca)))
      vca)

    (define/public (create-vca! name [level 1.0])
      (let ([v (new mixer-vca% [name name] [level level])])
        (add-vca! v)
        v))

    ;; ---- 路由 ----

    ;; 将音轨路由到总线
    (define/public (route-track-to-bus! track bus)
      (send bus add-track! track)
      this)

    ;; ---- 独奏逻辑 ----
    ;; 如果任何轨道有 solo，只有 solo 轨道发声

    (define (solo-active?)
      (ormap (lambda (t) (send t is-solo?)) tracks))

    ;; ---- 获取音轨增益 ----

    (define/public (get-track-gain track)
      (send track compute-gain (solo-active?)))

    ;; ---- 获取立体声增益 ----

    (define/public (get-track-stereo-gains track)
      (let ([g (get-track-gain track)])
        (let* ([angle (* (/ pi 4.0) (+ 1.0 (send track get-pan)))]
               [l (* (cos angle) g)]
               [r (* (sin angle) g)])
          (values l r))))

    ;; ---- MIDI 消息处理 ----

    ;; 将 MIDI 消息路由到指定轨道
    (define/public (route-midi-message track msg)
      (if (list? msg)
          (cond
            ;; Note On
            [(= (bitwise-and (car msg) #xF0) NOTE-ON)
             (let* ([vel (caddr msg)]
                    [scaled-vel (inexact->exact (round (* vel (get-track-gain track))))])
               (list (car msg) (cadr msg) (max 0 (min 127 scaled-vel))))]
            ;; 其他消息: 透传
            [else msg])
          msg))

    ;; ---- 实时输出 ----

    ;; 通过指定设备输出混音后的 MIDI 消息
    (define/public (play-note track device pitch velocity)
      (let ([g (get-track-gain track)])
        (when (> g 0.0)
          (let ([vel (inexact->exact (round (* velocity g)))])
            (send device note-on (send track get-channel) pitch (max 0 (min 127 vel)))))))

    (define/public (stop-note track device pitch)
      (send device note-off (send track get-channel) pitch))

    ;; ---- 采样混音 ----
    ;; 将多个音轨的事件列表混合成单一时间线

    (define/public (mix-events track-events-list)
      ;; track-events-list: ((track . events) ...)
      ;; 返回: ((time . event) ...) 按时间排序
      (let ([all '()])
        (for ([pair track-events-list])
          (let* ([track (car pair)]
                 [events (cdr pair)]
                 [g (get-track-gain track)])
            (when (> g 0.0)
              (for ([ev events])
                (set! all (cons (mix-event track ev g) all))))))
        (sort all < #:key car)))

    (define (mix-event track event gain)
      ;; 事件可能是 (time pitch vel dur) 或 (time event-list)
      (if (and (list? event) (number? (car event)))
          ;; 简单事件: (time data ...)
          (let ([time (car event)]
                [data (cdr event)])
            (cons time (scale-event-data data gain track)))
          event))

    (define (scale-event-data data gain track)
      (cond
        [(null? data) data]
        [(and (= (length data) 3)
              (= (bitwise-and (car data) #xF0) NOTE-ON))
         (let ([vel (caddr data)])
           (list (car data) (cadr data)
                 (max 0 (min 127 (inexact->exact (round (* vel gain)))))))]
        [else data]))

    ;; ---- 查看 ----

    (define/public (get-tracks) tracks)
    (define/public (get-buses) buses)
    (define/public (get-vcas) vcas)
    (define/public (get-master-bus) master-bus)
    (define/public (track-count) (length tracks))
    (define/public (bus-count) (length buses))

    (define/public (print-mixer)
      (printf "=== 混音台 ===\n")
      (printf "音轨: ~a  总线: ~a  VCA: ~a\n" (length tracks) (length buses) (length vcas))
      (for-each (lambda (t)
                  (printf "  ~a [CH~a] Vol:~a Pan:~a ~a~a\n"
                          (get-field name t)
                          (get-field channel t)
                          (~r (send t get-volume) #:min-width 4 #:precision 2)
                          (~r (send t get-pan) #:min-width 5 #:precision 2)
                          (if (send t is-muted?) " [M]" "")
                          (if (send t is-solo?) " [S]" "")))
                tracks)
      (printf "主总线: Vol:~a\n" (~r (send master-bus get-volume) #:min-width 4 #:precision 2)))
    ))

;; ============================================================
;; 便捷构造函数
;; ============================================================

(define (make-mixer)
  (new mixer%))

(define (make-track name [channel 0] [volume 1.0] [pan 0.0])
  (new mixer-track% [name name] [channel channel]
       [volume volume] [pan pan]))

(define (make-bus name [type 'submix])
  (new mixer-bus% [name name] [type type]))
