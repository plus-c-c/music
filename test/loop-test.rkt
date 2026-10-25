#lang racket

;; 循环展开：start/loop/end 三段、单遍修饰、同段叠加、踏板跨边界合并。
(require "../midi/main.rkt")

(define (assert v [msg "assertion failed"])
  (unless v (error 'loop-test msg)))

(define (event-with-pitch events pitch)
  (filter (lambda (e) (= (midi-event-pitch e) pitch)) events))

(define test-score
  (score
   (part "Main"
         (rest 1/8)                 ; 弱起 0.5
         (note 1/8 60 100 0)        ; 0.5–1.0
         (note 1/4 61 100 0)        ; 1.0–2.0
         (rest 1/2)                 ; 2.0–4.0
         (note 1/16 62 100 0)       ; 4.0–4.25
         (note 3/16 63 100 0)       ; 4.25–5.0
         (rest 1/4))
   (part "Melody"
         (rest 1/4)
         (note 1/4 80 100 2)        ; 循环内，但应排除
         (rest 1/2))
   (part "Pedal"
         (rest 1/8)
         (pedal 1/2)                ; 0.5–2.5
         (rest 1/4)
         (pedal 3/8)                ; 3.5–5.0
         (rest 1/4))))

(define-values (events pedals)
  (loop-score test-score 2
              #:start 1
              #:length 4
              #:end-length 1/2
              #:pickup-window 3/4
              #:exclude-voices '("Melody")
              #:bridge-pedals? #t))

;; 事件：第一遍 4 个（含弱起），第二遍 3 个（无弱起）。
(assert (= 7 (length events)))
(assert (= 1 (length (event-with-pitch events 60))))
(assert (= 2 (length (event-with-pitch events 61))))
(assert (= 2 (length (event-with-pitch events 62))))
(assert (= 2 (length (event-with-pitch events 63))))
(assert (null? (event-with-pitch events 80)))

;; 第二遍末尾 1/2 拍被清空：63 只到 8.5，后面的 8.5–9 是 rest。
(define last-63 (last (event-with-pitch events 63)))
(assert (= 8.25 (midi-event-start last-63)))
(assert (= 1/4 (midi-event-duration last-63)))
(assert (for/and ([e (in-list events)])
          (<= (+ (midi-event-start e) (midi-event-duration e)) 8.5)))

;; 踏板：0.5–2.5、3.5–6.5（跨边界合并）、7.5–8.5（末遍被裁短）。
(assert (= 3 (length pedals)))
(define p1 (list-ref pedals 0))
(define p2 (list-ref pedals 1))
(define p3 (list-ref pedals 2))
(assert (= 0.5 (pedal-event-start p1)))
(assert (= 2 (pedal-event-duration p1)))
(assert (= 3.5 (pedal-event-start p2)))
(assert (= 3 (pedal-event-duration p2)))
(assert (= 7.5 (pedal-event-start p3)))
(assert (= 1 (pedal-event-duration p3)))

;; 起始段可以独立修饰：清空弱起，60 不再出现。
(define-values (no-start-events no-start-pedals)
  (loop-score test-score 2
              #:start 1
              #:length 4
              #:end-length 1/2
              #:pickup-window 3/4
              #:start-transform empty-loop-segment
              #:exclude-voices '("Melody")
              #:bridge-pedals? #t))
(assert (= 6 (length no-start-events)))
(assert (null? (event-with-pitch no-start-events 60)))

;; 结束段可以叠加多个修饰方法：先清空，再让踏板覆盖同一段。
(define-values (held-events held-pedals)
  (loop-score test-score 2
              #:start 1
              #:length 4
              #:end-length 1/2
              #:pickup-window 3/4
              #:end-transform (list empty-loop-segment
                                   loop-segment-hold-pedal)
              #:exclude-voices '("Melody")
              #:bridge-pedals? #t))
(assert (= 7 (length held-events)))
(assert (= 3 (length held-pedals)))
(define held-last (last held-pedals))
(assert (= 7.5 (pedal-event-start held-last)))
(assert (= 3/2 (pedal-event-duration held-last)))
(assert (= 9 (for/fold ([m 0]) ([p (in-list held-pedals)])
               (max m (+ (pedal-event-start p) (pedal-event-duration p))))))

;; 只播放一遍时，start / end 两段仍然都应用修饰；
;; 同一个方法也可以同时用于 start 和 end。
(define-values (one-events one-pedals)
  (loop-score test-score 1
              #:start 1
              #:length 4
              #:end-length 1/2
              #:pickup-window 3/4
              #:start-transform loop-segment-hold-pedal
              #:end-transform loop-segment-hold-pedal
              #:exclude-voices '("Melody")
              #:bridge-pedals? #t))
(assert (= 3 (length one-events)))
(assert (null? (event-with-pitch one-events 60)))
(assert (= 2 (length one-pedals)))
(assert (= 5 (for/fold ([m 0]) ([p (in-list one-pedals)])
               (max m (+ (pedal-event-start p) (pedal-event-duration p))))))

(printf "PASS loop segments/n=1/pickup/exclude/end-pedal/bridge\n")
