#lang racket
(require "core.rkt")
(provide music-percussion%)
(provide music-perc-line%)
(define (rsound-length x)
  (- (rsound-stop x) (rsound-start x)))
(define (build-perc sound frame volumn lengthen)
  (rs-scale
   volumn
   (cond
     [(eq? lengthen #t) sound]
     [else
      (cond [(> (rsound-length sound) frame)
             (clip sound (rsound-start sound) (+ frame (rsound-start sound)))]
            [(= (rsound-length sound) frame)
             sound]
            [else
             (rs-append sound (silence (- frame (rsound-stop sound))))])]
     )))
(define (build-perc-line l current-frame)
  (if (eq? l '()) '()
      (cons
       (list (send (car l) construct) current-frame)
       (build-perc-line (cdr l) (+ current-frame (send (car l) get-frame))))))
(define music-percussion%
  (class* object% (music-perc-interface<%>)
    (init-field
     duration
     tempo
     timbre
     volumn
     lengthen)
    (super-new)
    (define/public (get-frame)
      (round (* duration tempo)))
    (define/public (set-lengthen new-lengthen)
      (new music-percussion%
           [duration duration]
           [tempo tempo]
           [timbre timbre]
           [volumn volumn]
           [lengthen new-lengthen]
           ))
    (define/public (set-duration new-duration)
      (new music-percussion%
           [duration new-duration]
           [tempo tempo]
           [timbre timbre]
           [volumn volumn]
           [lengthen lengthen]
           ))
    (define/public (set-tempo new-tempo)
      (new music-percussion%
           [duration duration]
           [tempo new-tempo]
           [timbre timbre]
           [volumn volumn]
           [lengthen lengthen]
           ))

    (define/public (set-timbre new-timbre)
      (new music-percussion%
           [duration duration]
           [tempo tempo]
           [timbre new-timbre]
           [volumn volumn]
           [lengthen lengthen]
           ))

    (define/public (set-volumn new-volumn)
      (new music-percussion%
           [duration duration]
           [tempo tempo]
           [timbre timbre]
           [volumn new-volumn]
           [lengthen lengthen]
           ))
    (define/public (scale-volumn scale)
      (set-volumn (* scale volumn)))

    (define/public (construct)
      (cond
        [(rsound? timbre) (build-perc timbre (get-frame) volumn lengthen)]
        [else
         (rs-scale volumn (timbre (round (* duration tempo))))]))
    ))

(define music-perc-line%
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
    (define/public (set-tempo new-tempo)
      (new music-perc-line%
           [root (send root set-tempo new-tempo)]
           [notes (map
                   (lambda (note)
                     (send note set-tempo new-tempo))
                   notes)]
           ))

    (define/public (set-timbre new-timbre)
      (new music-perc-line%
           [root (send root set-timbre new-timbre)]
           [notes (map
                   (lambda (note)
                     (send note set-timbre new-timbre))
                   notes)]
           ))
    (define/public (scale-volumn scale)
      (new music-perc-line%
           [root (send root scale-volumn scale)]
           [notes notes]
           ))
    (define/public (set-volumn new-volumn)
      (scale-volumn (/ new-volumn (get-field volumn root))))
    (define/private (build)
      (map
       (lambda (note)
         (rs-scale (get-field volumn root) (send note construct)))
       notes))
    (define/public (construct)
      (rs-append* (build)))
    ))
