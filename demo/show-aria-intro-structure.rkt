#lang racket

;; 展示 Aria-AMT 前奏 RKT 的音乐结构。
;; 默认：按小节列出声部音高/时值、tie 和 CC64 区间。
;; --full：额外显示带累计小节线的完整展开前谱面。

(require "../midi/main.rkt"
         "../midi-display.rkt"
         "kaitou-aria-intro.rkt"
         racket/cmdline
         racket/string)

(define full? #f)

(command-line
 #:program "show-aria-intro-structure"
 #:once-each
 [("--full") "额外显示带显式小节线的完整声部" (set! full? #t)]
 #:args () (void))

(define (base obj) (tie-base obj))

(define (tie-arrow obj)
  (cond
    [(and (tie-item? obj) (tie-item-from? obj) (tie-item-to? obj)) "↔"]
    [(and (tie-item? obj) (tie-item-to? obj)) "→"]
    [(and (tie-item? obj) (tie-item-from? obj)) "←"]
    [else ""]))

(define (item-text obj)
  (define b (base obj))
  (define arrow (tie-arrow obj))
  (cond
    [(midi-rest? b)
     (format "rest ~a" (duration-value (send b get-duration)))]
    [(midi-pedal? b)
     (format "pedal~a ~a" arrow (duration-value (send b get-duration)))]
    [(is-a? b midi-note%)
     (format "~a~a/~a" arrow
             (pitch-name (send b get-pitch))
             (duration-value (send b get-duration)))]
    [(or (is-a? b midi-chord%) (midi-cluster? b))
     (format "chord~a [~a]/~a" arrow
             (string-join
              (for/list ([n (in-list (chord-notes b))])
                (pitch-name (send n get-pitch)))
              " ")
             (duration-value (send b get-duration)))]
    [else "?"]))

(define (end-of-event e)
  (+ (midi-event-start e) (midi-event-duration e)))

(printf "=== Aria-AMT 前奏结构 ===\n")
(printf "tempo ~a BPM / PPQ ~a / bar-offset ~a 拍 / ~a 小节\n"
        tempo ppq bar-offset intro-bars)
(define all-events (score-events piece))
(define all-durations
  (for*/list ([v (in-list voices)]
              [ti (in-list (expand-voice (voice-items v)))])
    (beats->note-value (timed-item-duration ti))))
(printf "小节范围 ~a – ~a；实际发声 ~a – ~a；notes ~a；voices ~a；tie 片段 ~a；时值最大分母 ~a\n\n"
        bar-offset
        (apply max (map end-of-event all-events))
        (apply min (map midi-event-start all-events))
        (apply max (map end-of-event all-events))
        (length all-events)
        (length voices)
        (for*/sum ([v (in-list voices)]
                   [ti (in-list (expand-voice (voice-items v)))])
          (if (tie-item? (timed-item-object ti)) 1 0))
        (apply max (map denominator all-durations)))

(for ([v (in-list voices)])
  (define es (voice-events v))
  (define measures (filter score-measure? (voice-items v)))
  (printf "## ~a（~a events~a）\n"
          (voice-name v)
          (length es)
          (if (null? es)
              ""
              (format "，pitch ~a–~a"
                      (apply min (map midi-event-pitch es))
                      (apply max (map midi-event-pitch es)))))
  (cond
    [(null? measures)
     (for ([item (in-list (voice-items v))])
       (unless (midi-rest? (base item))
         (printf "  ~a\n" (item-text item))))]
    [else
     (for ([m (in-list measures)])
       (printf "  M~a  " (score-measure-number m))
       (define items (filter (lambda (x) (not (midi-rest? (base x))))
                             (score-measure-items m)))
       (if (null? items)
           (printf "（无音符）")
           (for ([item (in-list items)])
             (printf "~a  " (item-text item))))
       (newline))])
  (newline))

(printf "=== 踏板 CC64 ===\n")
(for ([p (in-list (score-pedals piece))])
  (printf "  ~a – ~a  (~a 拍)\n"
          (pedal-event-start p)
          (+ (pedal-event-start p) (pedal-event-duration p))
          (pedal-event-duration p)))

(when full?
  (printf "\n=== 完整谱面（| = 累计小节线；~ = tie）===\n")
  (for ([v (in-list voices)])
    (printf "\n--- ~a ---\n" (voice-name v))
    (print-voice-with-bars v #:bar-offset bar-offset)))
