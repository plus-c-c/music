#lang racket

;; ============================================================
;; midi-display.rkt — 只按“时值”显示声部，并显式标出小节线
;;
;; 不读取、也不打印 timed-item-start；start 只在 expand-voice
;; 内部作为中间量存在。显示时用累计时值决定小节线位置。
;; 跨小节 tie 用 ~ 表示：note ... ~ / ~ note ...
;; ============================================================

(require "midi-score.rkt"
         "midi-note.rkt"
         "midi-chord.rkt"
         "midi-perc.rkt"
         "midi-core.rkt"
         "midi-dsl.rkt"
         racket/string)

(define (pitch-name p)
  (define n (midi->note p))
  (format "~a~a" (car n) (cadr n)))

(define (duration-value d)
  (beats->note-value d))

(define (object->notation obj)
  (cond
    [(tie-item? obj)
     (string-append
      (if (tie-item-from? obj) "~ " "")
      (object->notation (tie-base obj))
      (if (tie-item-to? obj) " ~" ""))]
    [(midi-rest? obj)
     (format "rest ~a" (duration-value (send obj get-duration)))]
    [(midi-pedal? obj)
     (format "pedal ~a" (duration-value (send obj get-duration)))]
    [(is-a? obj midi-note%)
     (format "note ~a ~a"
             (pitch-name (send obj get-pitch))
             (duration-value (send obj get-duration)))]
    [(or (is-a? obj midi-chord%) (midi-cluster? obj))
     (format "chord ~a [~a]"
             (duration-value (send obj get-duration))
             (string-join
              (for/list ([n (in-list (chord-notes obj))])
                (pitch-name (send n get-pitch)))
              " "))]
    [(is-a? obj midi-perc%)
     (format "perc ~a ~a"
             (send obj get-drum)
             (duration-value (send obj get-duration)))]
    [else (format "~a" obj)]))

;; ------------------------------------------------------------
;; 通用：按累计时值插入小节线
;; ------------------------------------------------------------
(define (print-items-with-bars items
                               #:object [object-proc identity]
                               #:duration [duration-proc (lambda (o) (send o get-duration))]
                               #:bar-length [bar-length 4]
                               #:bar-offset [bar-offset 1]
                               #:port [port (current-output-port)])
  (define cursor 0)
  ;; 第一条小节线本身也要显示；之后每 bar-length 拍一条
  (define next-bar bar-offset)
  (define (barline!)
    (display "|\n" port))
  (define (advance-bars-before!)
    (let loop ()
      (when (>= cursor next-bar)
        (barline!)
        (set! next-bar (+ next-bar bar-length))
        (loop))))
  (define (advance-bars-after!)
    (let loop ()
      (when (> cursor next-bar)
        (barline!)
        (set! next-bar (+ next-bar bar-length))
        (loop))))
  (for ([obj (in-list items)])
    (advance-bars-before!)
    (display "  " port)
    (display (object->notation (object-proc obj)) port)
    (newline port)
    (set! cursor (+ cursor (duration-proc obj)))
    (advance-bars-after!))
  ;; 收尾小节线（声部正好结束在小节线上时）
  (advance-bars-before!))

(define (print-voice-with-bars v
                               #:bar-length [bar-length 4]
                               #:bar-offset [bar-offset 1]
                               #:port [port (current-output-port)])
  (print-items-with-bars (flatten-items (voice-items v))
                         #:bar-length bar-length
                         #:bar-offset bar-offset
                         #:port port))

(define (print-expanded-voice-with-bars ev
                                        #:bar-length [bar-length 4]
                                        #:bar-offset [bar-offset 1]
                                        #:port [port (current-output-port)])
  (print-items-with-bars
   (expanded-voice-items ev)
   #:object timed-item-object
   #:duration timed-item-duration
   #:bar-length bar-length
   #:bar-offset bar-offset
   #:port port))

(provide pitch-name
         duration-value
         object->notation
         print-items-with-bars
         print-voice-with-bars
         print-expanded-voice-with-bars)
