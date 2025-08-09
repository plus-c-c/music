#lang racket
(require "core.rkt")
(require "note.rkt")
(require "chord.rkt")
(require "perc.rkt")
(require "rest.rkt")
(require "volumn.rkt")
(require "env.rkt")
(provide (all-from-out "volumn.rkt"))
(require rsound)
(provide (all-from-out rsound))
(provide set-current-timbre)
(provide set-current-tempo)
(provide set-minium-fraction)
(provide music-play)
(provide PARTY)
(provide PERC-PARTY)
(provide NOTE)
(provide REST)
(provide PERC)
(provide CHORD)

(define (NOTE fraction pitch [volumn m] [timbre current-timbre] [tempo minium-duration])
  (new music-note%
       [pitch pitch]
       [duration (round (/ minium-fraction fraction))]
       [volumn volumn]
       [tempo tempo]
       [timbre timbre]))
(define (REST fraction [tempo minium-duration])
  (new music-rest%
       [duration (round (/ minium-fraction fraction))]
       [tempo tempo]
       [volumn m]))
(define (PERC fraction [timbre current-timbre] [volumn m] [lengthen #f] [tempo minium-duration])
  (new music-percussion%
       [duration (round (/ minium-fraction fraction))]
       [volumn volumn]
       [tempo tempo]
       [timbre timbre]
       [lengthen lengthen]))
(define (CHORD root type)
  (new music-chord%
       [root root]
       [type type]
       ))
(define (PARTY root x)
  (new music-line%
       [root root]
       [notes
        (map
         (lambda (y) (cond
            [(eq? (cadr y) 'rest)
             ; '((fraction volumn) rest) car=(fraction volumn) cadr='rest
             (REST (caar y))]
            [(number? (cadr y))
             ; '((fraction volumn) relative-pitch) car=(fraction volumn) cadr=relative-pitch
             (assemble-note root (caar y) (cadr y) (cadar y))]
            [(list? (cadr y))
             (cond
               [(symbol? (cadadr y))
                ; '((fraction volumn) (root type-symbol))
                ;car=(fraction volumn) cadr=(root type-symbol) caadr=root cadadr=type-symbol
                (CHORD
                 (assemble-note root (caar y) (caadr y) (cadar y))
                 (translate-chord (cadadr y)))]
               ; '((fraction volumn) (relative-pitches)) car=(fraction volumn) cadr=(relative-pitches)
               [else
                (CHORD
                 (assemble-note root (caar y) (caadr y) (cadar y))
                 (cadr y))])]
            ))
         (build-volumn root x)
         )]))
(define (PERC-PARTY x)
  (new music-perc-line%
       [root (REST 1)]
       [notes
        (map (lambda (y)
               (cond
                 [(eq? (cadr y) 'rest)
                  ; '((fraction volumn) rest) car=(fraction volumn) cadr='rest
                  (REST (caar y))]
                 [else
                  ; '((fraction volumn) timber)
                  (PERC (caar y) (cadr y) (cadar y))]))
             (build-volumn (REST 1) x))]))
