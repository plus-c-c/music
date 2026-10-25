#lang racket

;; 跨小节 tie：音符与踏板都应在小节线处合并，而不是重新触发。
(require "../midi/main.rkt"
         "../midi-display.rkt"
         racket/string)

(define (assert v [msg "assertion failed"])
  (unless v (error 'cross-measure-test msg)))

(define note-part
  (part "Tie Note"
        (measure 1
                 (rest 1/2)
                 (tie-start (note 1/2 60 90 0)))
        (measure 2
                 (tie-end (note 1/4 60 90 0))
                 (rest 3/4))))

(define note-events (score-events (score note-part)))
(assert (= 1 (length note-events)))
(assert (= 2 (midi-event-start (car note-events))))
(assert (= 3 (midi-event-duration (car note-events))))
(define tie-ti
  (for/first ([ti (in-list (expand-voice (voice-items note-part)))]
              #:when (tie-item? (timed-item-object ti)))
    ti))
(assert (string-contains? (object->notation (timed-item-object tie-ti)) "~"))

(define pedal-part
  (part "Tie Pedal"
        (measure 1
                 (rest 1/2)
                 (tie-start (pedal 1/2)))
        (measure 2
                 (tie-end (pedal 1/4))
                 (rest 3/4))))

(define pedal-events (score-pedals (score pedal-part)))
(assert (= 1 (length pedal-events)))
(assert (= 2 (pedal-event-start (car pedal-events))))
(assert (= 3 (pedal-event-duration (car pedal-events))))

(printf "PASS cross-measure note/pedal tie\n")
