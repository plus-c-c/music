#lang racket

;; ============================================================
;; quantize.rkt — MIDI 量化
;; 类似 Ardour 的量化功能
;; ============================================================
;;
;; 功能:
;;   - 网格量化 (将音符对齐到网格)
;;   - 摇摆量化 (Swing)
;;   - 力度量化
;;   - 人性化 (Humanize)
;;   - 递进量化 (可选强度)
;;   - 模板量化
;;

(require "midi-core.rkt"
         "midi-note.rkt"
         "midi-chord.rkt"
         "midi-perc.rkt")

(provide quantize
         swing
         humanize
         velocity-quantize
         quantize-grid
         swing-grid
         make-quantize-params)

;; ============================================================
;; 量化参数
;; ============================================================

(struct quantize-params
  (grid          ; 量化网格 (拍): 1.0=四分音符, 0.5=八分音符, 0.25=十六分音符
   swing         ; 摇摆量 0.0 ~ 1.0
   strength      ; 量化强度 0.0 ~ 1.0
   velocity-strength ; 力度量化强度 0.0 ~ 1.0
   humanize      ; 人性化量 0.0 ~ 1.0
   offset)       ; 时间偏移 (拍)
  #:transparent)

(define (make-quantize-params [grid 0.25]
                               [swing 0.0]
                               [strength 1.0]
                               [vel-strength 0.0]
                               [humanize 0.0]
                               [offset 0.0])
  (quantize-params grid swing strength vel-strength humanize offset))

;; ============================================================
;; 核心量化函数
;; ============================================================

;; 网格量化: 将事件时间对齐到最近的网格点
(define (quantize events params)
  (define grid (quantize-params-grid params))
  (define strength (quantize-params-strength params))
  (define offset (quantize-params-offset params))

  (map (lambda (ev)
         (cond
           [(is-a? ev midi-note%)
            (quantize-note ev grid strength offset)]
           [(is-a? ev midi-chord%)
            (quantize-chord ev grid strength offset)]
           [(is-a? ev midi-perc%)
            (quantize-perc ev grid strength offset)]
           [(list? ev)
            (map (lambda (e) (quantize (list e) params)) ev)]
           [else ev]))
       events))

;; 摇摆量化
(define (swing events params)
  (define grid (quantize-params-grid params))
  (define swing-amount (quantize-params-swing params))
  (define strength (quantize-params-strength params))

  (map (lambda (ev)
         (cond
           [(is-a? ev midi-note%)
            (swing-note ev grid swing-amount strength)]
           [(is-a? ev midi-chord%)
            (swing-chord ev grid swing-amount strength)]
           [else ev]))
       events))

;; 人性化: 添加随机偏移
(define (humanize events params)
  (define amount (quantize-params-humanize params))
  (define vel-amount (quantize-params-velocity-strength params))

  (map (lambda (ev)
         (cond
           [(is-a? ev midi-note%)
            (humanize-note ev amount vel-amount)]
           [(is-a? ev midi-chord%)
            (humanize-chord ev amount vel-amount)]
           [else ev]))
       events))

;; 力度量化: 将力度对齐到网格
(define (velocity-quantize events target-vel [strength 1.0])
  (map (lambda (ev)
         (cond
           [(is-a? ev midi-note%)
            (let* ([vel (send ev get-velocity)]
                   [diff (- target-vel vel)]
                   [new-vel (inexact->exact
                             (round (+ vel (* diff strength))))])
              (send ev scale-velocity (/ new-vel (max 1 vel))))]
           [(is-a? ev midi-chord%)
            (let* ([vel (send ev get-velocity)]
                   [diff (- target-vel vel)]
                   [new-vel (inexact->exact
                             (round (+ vel (* diff strength))))])
              (send ev set-velocity new-vel))]
           [else ev]))
       events))

;; ============================================================
;; 内部量化函数
;; ============================================================

(define (quantize-note note grid strength offset)
  (let* ([dur (send note get-duration)]
         ;; 简化: 假设音符从 0 开始 (需要配合事件时间使用)
         [target-dur (round-to-grid dur grid)]
         [new-dur (+ dur (* (- target-dur dur) strength))])
    (send note set-duration (max 0.01 new-dur))))

(define (quantize-chord chord grid strength offset)
  (let* ([dur (send chord get-duration)]
         [target-dur (round-to-grid dur grid)]
         [new-dur (+ dur (* (- target-dur dur) strength))])
    (send chord set-duration (max 0.01 new-dur))))

(define (quantize-perc perc grid strength offset)
  (let* ([dur (send perc get-duration)]
         [target-dur (round-to-grid dur grid)]
         [new-dur (+ dur (* (- target-dur dur) strength))])
    (send perc set-duration (max 0.01 new-dur))))

(define (swing-note note grid swing-amount strength)
  (let* ([dur (send note get-duration)]
         ;; 摇摆: 偶数位置延迟
         [grid-pos (modulo (inexact->exact (round (/ dur grid))) 2)]
         [swing-offset (if (= grid-pos 1)
                           (* grid swing-amount 0.5)
                           0.0)]
         [new-dur (+ dur (* swing-offset strength))])
    (send note set-duration (max 0.01 new-dur))))

(define (swing-chord chord grid swing-amount strength)
  (let* ([dur (send chord get-duration)]
         [grid-pos (modulo (inexact->exact (round (/ dur grid))) 2)]
         [swing-offset (if (= grid-pos 1)
                           (* grid swing-amount 0.5)
                           0.0)]
         [new-dur (+ dur (* swing-offset strength))])
    (send chord set-duration (max 0.01 new-dur))))

(define (humanize-note note amount vel-amount)
  (let* ([dur (send note get-duration)]
         [vel (send note get-velocity)]
         [dur-offset (* (- (random) 0.5) 2.0 amount)]
         [vel-offset (inexact->exact
                      (round (* (- (random) 0.5) 2.0 127.0 vel-amount)))])
    (send note set-duration (max 0.01 (+ dur dur-offset)))
    (send note set-velocity (max 1 (min 127 (+ vel vel-offset))))))

(define (humanize-chord chord amount vel-amount)
  (let* ([dur (send chord get-duration)]
         [vel (send chord get-velocity)]
         [dur-offset (* (- (random) 0.5) 2.0 amount)]
         [vel-offset (inexact->exact
                      (round (* (- (random) 0.5) 2.0 127.0 vel-amount)))])
    (send chord set-duration (max 0.01 (+ dur dur-offset)))
    (send chord set-velocity (max 1 (min 127 (+ vel vel-offset))))))

;; 辅助: 四舍五入到最近的网格点
(define (round-to-grid value grid)
  (* grid (round (/ value grid))))

;; ============================================================
;; 预设网格
;; ============================================================

(define quantize-grid
  (hash 'whole 4.0
        'half 2.0
        'quarter 1.0
        'eighth 0.5
        'sixteenth 0.25
        'thirty-second 0.125
        'triplet-quarter (/ 4.0 3.0)
        'triplet-eighth (/ 2.0 3.0)
        'triplet-sixteenth (/ 1.0 3.0)
        'dotted-quarter 1.5
        'dotted-eighth 0.75
        'dotted-sixteenth 0.375))

(define swing-grid
  (hash 'none 0.0
        'light 0.25
        'medium 0.5
        'heavy 0.75
        'exaggerated 1.0))
