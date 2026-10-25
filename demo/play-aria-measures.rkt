#lang racket

;; 按小节播放 Aria-AMT 前奏。
;; 第一段包含弱起 + M1；随后 M2、M3、M4。
;; 每段保留跨区间长音/踏板的剩余部分，并在段间留出短暂停顿。

(require "../midi/main.rkt"
         "kaitou-aria-intro.rkt"
         racket/string)

(define gap-seconds 0.6)

(define (slice-events start end)
  (for/list ([e (in-list (score-events piece))]
             #:when (and (< (midi-event-start e) end)
                         (> (+ (midi-event-start e) (midi-event-duration e)) start)))
    (define s (max start (midi-event-start e)))
    (define e* (min end (+ (midi-event-start e) (midi-event-duration e))))
    (midi-event (- s start) (- e* s)
                (midi-event-pitch e)
                (midi-event-velocity e)
                (midi-event-channel e)
                (midi-event-kind e))))

(define (slice-pedals start end)
  (for/list ([p (in-list (score-pedals piece))]
             #:when (and (< (pedal-event-start p) end)
                         (> (+ (pedal-event-start p) (pedal-event-duration p)) start)))
    (define s (max start (pedal-event-start p)))
    (define e* (min end (+ (pedal-event-start p) (pedal-event-duration p))))
    (pedal-event (- s start) (- e* s)
                 (pedal-event-value p)
                 (pedal-event-channel p))))

(define (play-range label start end)
  (printf "~a：~a – ~a 拍\n" label start end)
  (play-events (slice-events start end) (length voices)
               #:tempo tempo
               #:backend 'audio
               #:pedals (slice-pedals start end))
  (sleep gap-seconds))

(define ranges
  (list
   (list "弱起 + M1" 0 (+ bar-offset 4))
   (list "M2" (+ bar-offset 4) (+ bar-offset 8))
   (list "M3" (+ bar-offset 8) (+ bar-offset 12))
   (list "M4" (+ bar-offset 12) (+ bar-offset 16))))

(for ([r (in-list ranges)] [i (in-naturals 1)])
  (printf "\n[~a/~a] " i (length ranges))
  (apply play-range r))
