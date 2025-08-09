#lang racket
(require "core.rkt")
(provide music-note%)
(provide music-line%)
(define music-note%
  (class* object% (music-note-interface<%>)
    (init-field
     pitch
     duration
     tempo
     timbre
     volumn
     )
    (super-new)
    (define/public (get-frame)
      (round (* duration tempo)))
    (define/public (set-pitch new-pitch)
      (new music-note%
           [pitch new-pitch]
           [duration duration]
           [tempo tempo]
           [timbre timbre]
           [volumn volumn]
           ))
    (define/public (set-root new-pitch) (set-pitch new-pitch))
    (define/public (rise-pitch change-of-pitch) (set-pitch (+ pitch change-of-pitch)))
    (define/public (rise-root change-of-pitch) (set-pitch (+ pitch change-of-pitch)))

    (define/public (set-duration new-duration)
      (new music-note%
           [pitch pitch]
           [duration new-duration]
           [tempo tempo]
           [timbre timbre]
           [volumn volumn]
           ))
    (define/public (set-tempo new-tempo)
      (new music-note%
           [pitch pitch]
           [duration duration]
           [tempo new-tempo]
           [timbre timbre]
           [volumn volumn]
           ))

    (define/public (set-timbre new-timbre)
      (new music-note%
           [pitch pitch]
           [duration duration]
           [tempo tempo]
           [timbre new-timbre]
           [volumn volumn]
           ))

    (define/public (set-volumn new-volumn)
      (new music-note%
           [pitch pitch]
           [duration duration]
           [tempo tempo]
           [timbre timbre]
           [volumn new-volumn]
           ))
    (define/public (scale-volumn scale)
      (set-volumn (* scale volumn)))

    (define/public (construct) (rs-scale volumn (timbre pitch (round (* duration tempo)))))
    ))
(define music-line%
  (class* object% (music-interface<%>)
    (super-new)
    (init-field
     root
     notes)
    (define/public (get-frame)
      (apply + (map
                (lambda (note)
                  (send note get-frame))
                notes))
      )
    (define/public (rise-root change-of-pitch)
      (new music-line%
           [root (send root rise-root change-of-pitch)]
           [notes (map
                   (lambda (note)
                     (send note rise-root change-of-pitch))
                   notes)]
           ))
    (define/public (set-root new-pitch)
      (rise-root (- new-pitch (get-field pitch root))))

    (define/public (set-tempo new-tempo)
      (new music-line%
           [root (send root set-tempo new-tempo)]
           [notes (map
                   (lambda (note)
                     (send note set-tempo new-tempo))
                   notes)]
           ))

    (define/public (set-timbre new-timbre)
      (new music-line%
           [root (send root set-timbre new-timbre)]
           [notes (map
                   (lambda (note)
                     (send note set-timbre new-timbre))
                   notes)]
           ))
    (define/public (scale-volumn scale)
      (new music-line%
           [root (send root scale-volumn scale)]
           [notes notes]
           ))
    (define/public (set-volumn new-volumn)
      (scale-volumn (/ new-volumn (get-field volumn root))))
    (define/private (build)
      (map (lambda (note) (rs-scale (get-field volumn root) (send note construct)))
           notes))
    (define/public (construct)
      (rs-append* (build)))
    ))
