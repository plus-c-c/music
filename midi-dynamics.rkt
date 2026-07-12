#lang racket

(require "midi-core.rkt"
         "midi-note.rkt"
         "midi-chord.rkt"
         "midi-perc.rkt")

;; ==================== 力度曲线函数 ====================

;; 线性插值
(define (lerp a b t)
  (+ a (* (- b a) t)))

;; 将值限制在范围内
(define (clamp val lo hi)
  (max lo (min hi val)))

;; ==================== 乐句力度控制 ====================

;; 渐强: 从 start-vel 线性增长到 end-vel
(define (crescendo notes start-vel [end-vel 127])
  (define n (length notes))
  (if (<= n 1)
      notes
      (for/list ([note notes]
                 [i (in-range n)])
        (define t (/ i (- n 1)))
        (define vel (round (lerp start-vel end-vel t)))
        (apply-velocity note vel))))

;; 渐弱: 从 start-vel 线性降低到 end-vel
(define (decrescendo notes start-vel [end-vel 1])
  (crescendo notes start-vel end-vel))

;; 亚历山大 (先渐强再渐弱)
(define (crescendo-decrescendo notes peak-vel [start-vel 1])
  (define n (length notes))
  (if (<= n 1)
      notes
      (let* ([half (quotient n 2)]
             [first-half (take notes half)]
             [second-half (drop notes half)])
        (append
         (crescendo first-half start-vel peak-vel)
         (decrescendo second-half peak-vel start-vel)))))

;; 保持固定力度
(define (constant-velocity notes vel)
  (for/list ([note notes])
    (apply-velocity note vel)))

;; 增强: 将每个音符的力度乘以 factor
(define (accent notes factor)
  (for/list ([note notes])
    (scale-velocity-with-limit note factor)))

;; 弱化: 将每个音符的力度乘以 factor
(define (diminuendo notes factor)
  (accent notes factor))

;; 渐强后突弱 (sforzando 效果)
(define (sforzando notes accent-vel [normal-vel 60] [accent-pos 0])
  (define n (length notes))
  (for/list ([note notes]
             [i (in-range n)])
    (if (= i accent-pos)
        (apply-velocity note accent-vel)
        (apply-velocity note normal-vel))))

;; 指定力度标记 (ff, f, mf, m, mp, p, pp)
(define dynamics
  (hasheq
   'pp 30
   'p  50
   'mp 70
   'm  85
   'mf 100
   'f  115
   'ff 127))

(define (apply-dynamic notes dyn)
  (define vel (hash-ref dynamics dyn #f))
  (unless vel
    (error 'apply-dynamic "未知力度标记: ~a (可用: pp p mp m mf f ff)" dyn))
  (constant-velocity notes vel))

;; ==================== 应用力度的辅助函数 ====================

;; 给音符对象设置力度 (返回新对象)
(define (apply-velocity note vel)
  (define clamped (clamp (round vel) 1 127))
  (cond
    [(is-a? note midi-note%)
     (send note set-velocity clamped)]
    [(is-a? note midi-chord%)
     (send note set-velocity clamped)]
    [(is-a? note midi-perc%)
     (send note set-velocity clamped)]
    [else note]))

;; 缩放力度 (返回新对象)
(define (scale-velocity-with-limit note factor)
  (cond
    [(is-a? note midi-note%)
     (send note scale-velocity factor)]
    [(is-a? note midi-chord%)
     (define vel (round (* (send note get-velocity) factor)))
     (send note set-velocity (clamp vel 1 127))]
    [(is-a? note midi-perc%)
     (define vel (round (* (send note get-velocity) factor)))
     (send note set-velocity (clamp vel 1 127))]
    [else note]))

(provide crescendo decrescendo crescendo-decrescendo
         constant-velocity accent diminuendo
         sforzando apply-dynamic
         apply-velocity scale-velocity-with-limit dynamics)
