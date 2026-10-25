#lang racket

;; ============================================================
;; automation.rkt — 参数自动化
;; 类似 Ardour 的自动化车道
;; ============================================================
;;
;; 功能:
;;   - 关键帧自动化 (线性/阶梯/曲线插值)
;;   - 支持音量/声像/效果参数
;;   - 自动化曲线编辑
;;   - MIDI CC 自动化
;;

(require "midi-core.rkt")

(provide automation%
         automation-point%
         make-automation
         add-point
         get-value-at
         linear-interpolate
         step-interpolate
         curve-interpolate)

;; ============================================================
;; automation-point% — 自动化关键帧
;; ============================================================
(define automation-point%
  (class object%
    (super-new)

    (init-field [beat 0.0]       ; 时间位置 (拍)
                [value 0.0]      ; 参数值 (0.0 ~ 1.0)
                [curve-type 'linear])  ; 'linear, 'step, 'curve

    (define/public (get-beat) beat)
    (define/public (get-value) value)
    (define/public (get-curve-type) curve-type)

    (define/public (set-beat! b) (set! beat b) this)
    (define/public (set-value! v) (set! value (max 0.0 (min 1.0 v))) this)
    (define/public (set-curve-type! ct) (set! curve-type ct) this)
    ))

;; ============================================================
;; automation% — 自动化车道
;; ============================================================
(define automation%
  (class object%
    (super-new)

    (init-field [name "Volume"]   ; 参数名
                [param-range (cons 0.0 1.0)] ; 参数范围
                [default-value 0.5]
                [cc-number #f])   ; MIDI CC 编号 (如 7=音量)

    (field [points '()]          ; 关键帧列表 (按时间排序)
           [enabled? #t])

    ;; ---- 关键帧管理 ----

    (define/public (add-point! beat value [curve-type 'linear])
      (let ([pt (new automation-point%
                     [beat beat]
                     [value value]
                     [curve-type curve-type])])
        (set! points (sort (cons pt points) < #:key (lambda (p) (send p get-beat))))
        this))

    (define/public (remove-point-at beat)
      (set! points (filter (lambda (p) (not (= (send p get-beat) beat))) points))
      this)

    (define/public (clear-points!)
      (set! points '())
      this)

    (define/public (get-points) points)

    (define/public (point-count) (length points))

    ;; ---- 求值 ----

    ;; 获取指定拍的参数值
    (define/public (get-value-at beat)
      (if (null? points)
          default-value
          (let* ([before (filter (lambda (p) (<= (send p get-beat) beat)) points)]
                 [after (filter (lambda (p) (> (send p get-beat) beat)) points)])
            (cond
              ;; 无前面的点
              [(null? before) (send (car points) get-value)]
              ;; 无后面的点
              [(null? after) (send (last before) get-value)]
              ;; 两点之间插值
              [else
               (let* ([p1 (last before)]
                      [p2 (car after)]
                      [t1 (send p1 get-beat)]
                      [t2 (send p2 get-beat)]
                      [v1 (send p1 get-value)]
                      [v2 (send p2 get-value)]
                      [curve (send p1 get-curve-type)]
                      [progress (if (= t2 t1) 0.0 (/ (- beat t1) (- t2 t1)))])
                 (case curve
                   [(linear) (linear-interpolate v1 v2 progress)]
                   [(step) (step-interpolate v1 v2 progress)]
                   [(curve) (curve-interpolate v1 v2 progress)]
                   [else (linear-interpolate v1 v2 progress)]))]))))

    ;; 获取指定拍的 MIDI CC 值 (0~127)
    (define/public (get-cc-at beat)
      (inexact->exact (round (* (get-value-at beat) 127.0))))

    ;; ---- 自动化曲线生成 ----
    ;; 生成从 start-beat 到 end-beat 的所有 CC 消息

    (define/public (generate-cc-messages start-beat end-beat [resolution 0.1])
      (if cc-number
          (let ([result '()])
            (let loop ([beat start-beat])
              (when (<= beat end-beat)
                (let ([value (get-cc-at beat)])
                  (set! result (cons (list beat cc-number value) result)))
                (loop (+ beat resolution))))
            (reverse result))
          '()))

    ;; 生成原始值序列
    (define/public (sample-values start-beat end-beat [resolution 0.1])
      (let loop ([beat start-beat] [result '()])
        (if (> beat end-beat)
            (reverse result)
            (loop (+ beat resolution)
                  (cons (cons beat (get-value-at beat)) result)))))

    ;; ---- 启用/禁用 ----

    (define/public (enable!) (set! enabled? #t) this)
    (define/public (disable!) (set! enabled? #f) this)
    (define/public (is-enabled?) enabled?)

    ;; ---- 获取/设置参数 ----

    (define/public (get-name) name)
    (define/public (get-cc-number) cc-number)
    (define/public (set-cc-number! cc) (set! cc-number cc) this)
    ))

;; ============================================================
;; 插值函数
;; ============================================================

;; 线性插值
(define (linear-interpolate v1 v2 t)
  (+ v1 (* (- v2 v1) t)))

;; 阶梯插值 (在 t=0 时跳变)
(define (step-interpolate v1 v2 t)
  (if (< t 1.0) v1 v2))

;; 曲线插值 (S 型曲线)
(define (curve-interpolate v1 v2 t)
  (let* ([s (* t t (- 3.0 (* 2.0 t)))])  ; smoothstep
    (+ v1 (* (- v2 v1) s))))

;; ============================================================
;; 便捷函数
;; ============================================================

(define (make-automation name [cc #f] [default 0.5])
  (new automation% [name name] [cc-number cc] [default-value default]))

(define (add-point auto beat value [curve 'linear])
  (send auto add-point! beat value curve)
  auto)

(define (get-value-at auto beat)
  (send auto get-value-at beat))
