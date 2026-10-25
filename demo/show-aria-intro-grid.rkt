#lang racket

;; 检查 Aria-AMT 前奏各声部在 1/16 全音符网格上的对齐情况；
;; 单独列出反拍（1/2 拍位）和十六分弱位（1/4、3/4 拍位）。

(require "../midi/main.rkt"
         "../midi-display.rkt"
         "kaitou-aria-intro.rkt"
         racket/string)

(define grid 1/4) ; 1/4 拍 = 1/16 全音符

(define (grid-aligned? x)
  (define q (/ x grid))
  (and (number? q)
       (< (abs (- q (round q))) 1e-9)))

(define (base obj) (tie-base obj))

(define (attack? obj)
  (not (and (tie-item? obj) (tie-item-from? obj))))

(define (fraction-part x)
  (- x (floor x)))

(define (category pos)
  (define f (fraction-part pos))
  (cond
    [(= f 0) "on-beat"]
    [(= f 1/2) "反拍(1/2)"]
    [(or (= f 1/4) (= f 3/4)) "弱位(1/4,3/4)"]
    [else "off-grid"]))

(define (pos-string x)
  (if (integer? x)
      (format "~a" x)
      (format "~a" (exact->inexact x))))

(define (measure-position start)
  (if (< start bar-offset)
      (values "Pickup" start)
      (let* ([rel (- start bar-offset)]
             [m (inexact->exact (floor (/ rel 4)))]
             [pos (- rel (* m 4))])
        (values (format "M~a" (add1 m)) pos))))

(define (pitches-text obj)
  (define b (base obj))
  (cond
    [(is-a? b midi-note%)
     (pitch-name (send b get-pitch))]
    [(or (is-a? b midi-chord%) (midi-cluster? b))
     (string-join
      (for/list ([n (in-list (chord-notes b))])
        (pitch-name (send n get-pitch)))
      "+")]
    [else "?"]))

(define (note-or-chord? obj)
  (define b (base obj))
  (or (is-a? b midi-note%) (is-a? b midi-chord%) (midi-cluster? b)))

(printf "=== 网格对齐与反拍检查 ===\n")
(printf "grid = ~a 拍 = 1/16 全音符；bar-offset = ~a 拍\n\n" grid bar-offset)

(define all-timed
  (for*/list ([v (in-list voices)]
              [ti (in-list (expand-voice (voice-items v)))])
    ti))

(define bad-grid
  (for/list ([ti (in-list all-timed)]
             #:unless (and (grid-aligned? (timed-item-start ti))
                           (grid-aligned? (timed-item-duration ti))))
    ti))

(printf "所有 note/rest/pedal 片段的 start/duration 都在网格上：~a\n"
        (if (null? bad-grid) "是" "否"))
(unless (null? bad-grid)
  (for ([ti (in-list bad-grid)])
    (printf "  OFF-GRID start=~a dur=~a ~a\n"
            (timed-item-start ti)
            (timed-item-duration ti)
            (object->notation (timed-item-object ti)))))

(for ([v (in-list voices)])
  (define attacks
    (for/list ([ti (in-list (expand-voice (voice-items v)))]
               #:when (and (note-or-chord? (timed-item-object ti))
                           (attack? (timed-item-object ti))))
      ti))
  (define rows
    (for/list ([ti (in-list attacks)])
      (define start (timed-item-start ti))
      (define-values (m pos) (measure-position start))
      (list m pos (category pos) (pitches-text (timed-item-object ti)) start)))
  (define on (for/sum ([r (in-list rows)] #:when (equal? (caddr r) "on-beat")) 1))
  (define off8 (for/sum ([r (in-list rows)] #:when (equal? (caddr r) "反拍(1/2)")) 1))
  (define off16 (for/sum ([r (in-list rows)] #:when (equal? (caddr r) "弱位(1/4,3/4)")) 1))
  (printf "\n## ~a\n" (voice-name v))
  (printf "attacks=~a  on-beat=~a  反拍(1/2)=~a  弱位(1/4,3/4)=~a\n"
          (length rows) on off8 off16)
  (for ([m (in-list (remove-duplicates (map car rows)))])
    (define rs (filter (lambda (r) (equal? (car r) m)) rows))
    (define off8s (filter (lambda (r) (equal? (caddr r) "反拍(1/2)")) rs))
    (define off16s (filter (lambda (r) (equal? (caddr r) "弱位(1/4,3/4)")) rs))
    (when (pair? off8s)
      (printf "  ~a 反拍(1/2)：" m)
      (for ([r (in-list off8s)])
        (printf "  ~a ~a" (pos-string (cadr r)) (cadddr r)))
      (newline))
    (when (pair? off16s)
      (printf "  ~a 弱位(1/4,3/4)：" m)
      (for ([r (in-list off16s)])
        (printf "  ~a ~a" (pos-string (cadr r)) (cadddr r)))
      (newline))))

(printf "\n=== 踏板网格 ===\n")
(for ([p (in-list (score-pedals piece))])
  (printf "on ~a dur ~a off ~a  grid: ~a/~a\n"
          (pedal-event-start p)
          (pedal-event-duration p)
          (+ (pedal-event-start p) (pedal-event-duration p))
          (if (grid-aligned? (pedal-event-start p)) "OK" "BAD")
          (if (grid-aligned? (pedal-event-duration p)) "OK" "BAD")))
