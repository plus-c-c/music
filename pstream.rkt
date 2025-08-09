#lang racket
(require rsound)
(define music-track%
  (class object%
    (super-new)

    (field
     [track (make-pstream)]
     [current-frame (round (* 0.05 (default-sample-rate)))]
     )

    (define/public (add-queue segment)
      (pstream-queue track (send segment construct) current-frame)
      (set! current-frame (+ current-frame (send segment get-time))))

    (define/public (play)
      (pstream-play track (silence current-frame)))
))
(provide music-track%)
