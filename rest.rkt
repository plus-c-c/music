#lang racket
(require "core.rkt")
(provide music-rest%)
(define music-rest%
  (class* object% (music-note-interface<%> music-perc-interface<%>)
    (super-new)
    (init-field
     tempo
     duration
     volumn)
    (define/public (get-frame)
      (round (* duration tempo)))
    (define/public (set-duration new-duration)
      (new music-rest%
           [duration new-duration]
           [tempo tempo]
           ))
    (define/public (set-tempo new-tempo)
      (new music-rest%
           [duration duration]
           [tempo new-tempo]
           ))
    (define/public (set-root x) this)
    (define/public (rise-root x) this)
    (define/public (set-timbre x) this)
    (define/public (set-volumn x) this)
    (define/public (scale-volumn x) this)
    (define/public (set-lengthen x) this)
    (define/public (construct)
      (silence (round (* tempo duration))))
    )
  )
