#lang racket
(require "core.rkt")
(require "env.rkt")
(require "syntax.rkt")
(provide music-chord%)
(define music-chord%
  (class* object% (music-note-interface<%>)
    (super-new)
    (init-field
     root
     type)
    (define/public (get-frame)
      (send root get-frame))
    (define/public (set-root new-pitch)
      (new music-chord%
           [root (send root set-pitch new-pitch)]
           [type type]))
    (define/public (rise-root change-of-pitch)
      (set-root (+ change-of-pitch (get-field pitch root))))

    (define/public (set-duration new-duration)
      (new music-chord%
           [root (send root set-duration new-duration)]
           [type type]))
    (define/public (set-tempo new-tempo)
      (new music-chord%
           [root (send root set-tempo new-tempo)]
           [type type]))

    (define/public (set-timbre new-timbre)
      (new music-chord%
           [root (send root set-timbre new-timbre)]
           [type type]))

    (define/public (set-volumn new-volumn)
      (new music-chord%
           [root (send root set-volumn new-volumn)]
           [type type]))
    (define/public (scale-volumn scale)
      (new music-chord%
           [root (send root scale-volumn scale)]
           [type type]))

    (define/private (build)
      (map (lambda (relative-pitch)
             (send
              (send
               (send root rise-root (- relative-pitch 1))
               set-volumn ;(sqrt (/ (get-field volumn root) (length type)) )
               (get-field volumn root)
               )
              construct))
           type))

    (define/public (construct)
      (rs-overlay* (build)))
    ))
;;code translated from racket-music https://github.com/alphajuliet/racket-music/blob/master/chord.rkt
(define chords
  (hash 'major   '(1 5 8)
        'minor   '(1 4 8)
        'dim     '(1 4 7)
        'aug     '(1 5 9)
        'x4+5    '(1 6 8)
        'maj4    '(1 5 6)
        'sus4    '(1 6 8)
        'min4    '(1 4 6)
        'maj6    '(1 5 10)
        'min6    '(1 4 10)
        'aug4    '(1 6 9)
        'x4+6    '(1 6 10)
        'b5      '(1 5 7)
        'min#5   '(1 4 9)


        'x7      '(1 5 8 11)
        'maj7    '(1 5 8 12)
        'aug7    '(1 5 9 11)
        'augmaj7 '(1 5 9 12)
        'min7    '(1 4 8 11)
        'minmaj7 '(1 4 8 12)
        'dim7    '(1 4 7 10)
        'dimmin7 '(1 4 7 11)
        'x7b5    '(1 5 7 11)

        'x9      '(1 5 8 11 15)
        'maj9    '(1 5 8 12 15)
        'min9    '(1 4 8 11 15)
        'minmaj9 '(1 4 8 12 15)
        'add9    '(1 5 8 15)
        'minadd9 '(1 4 8 15)

        'maj+2   '(1 3 5 8)
        'min+2   '(1 3 4 8)
        'mixed3  '(1 4 5 8)
        'sus2+4  '(1 3 6 8)
        'maj4+6  '(1 5 6 10)
        'min4+6  '(1 4 6 9)
        'min+6   '(1 4 8 10)
        'minaug7 '(1 4 9 12)))
(define (translate-chord x)
  (hash-ref chords x))
(define (assemble-note root fraction relative-pitch volumn)
  (set-volumn
   (set-duration
    (rise-root root (- relative-pitch 1))
    (round (/ minium-fraction fraction)))
   volumn))
(define (build-volumn root x)
  (cond
     [(eq? x '()) '()]
     [(list? (caar x))
      ;car = (fraction volumn)
      (cons (car x)
            (build-volumn
             (set-volumn root (cadaar x))
             (cdr x)))]
     [else
      (cons (cons
             (cons (caar x) (cons (get-volumn root) '()))
             (cdar x))
            (build-volumn
             root
             (cdr x)))]))
(provide build-volumn)
(provide translate-chord)
(provide assemble-note)
