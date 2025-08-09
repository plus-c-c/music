#lang racket
(require rsound)
(provide music-play)
(provide (all-from-out rsound))
(define (music-play segment)
  (play (send segment construct)))
(define music-interface<%>
  (interface ()
    set-tempo
    set-timbre
    set-volumn
    scale-volumn
    get-frame
    construct
    ))
(define music-note-interface<%>
  (interface (music-interface<%>)
    rise-root
    set-root
    ))
(define music-perc-interface<%>
  (interface ()
    set-lengthen))
(provide music-interface<%>)
(provide music-note-interface<%>)
(provide music-perc-interface<%>)

(define music-bar%
  (class object%
    (super-new)
    (init-field lines)
    (define/public (get-time)
      (send (car lines) get-time)
      )
    (define/private (build)
      (map (lambda (line) (send line construct))
           lines))
    (define/public (construct)
      (rs-overlay* (build))
      )
    )
  )
