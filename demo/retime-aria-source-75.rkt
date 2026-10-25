#lang racket

;; Aria-AMT 的原始快照被按 76 BPM 写进了 MIDI；原曲实际拍长更接近 75 BPM。
;; 这里把现有 76 BPM MIDI 的时间轴机械重定时为 75 BPM，音高/力度/踏板值不变。

(require "../midi-reader.rkt"
         "../midi-file.rkt")

(define in-midi "demo/reference/aria-amt-full-76.mid")
(define out-midi "demo/reference/aria-amt-full-75.mid")
(define source-tempo 76)
(define target-tempo 75)
(define ppq 480)

(define data (read-midi-data in-midi))
(define (scale-tick tick)
  (inexact->exact (round (* tick (/ target-tempo source-tempo)))))

(define mf (make-midi-file 0 ppq target-tempo))
(send mf add-track-name "Aria-AMT full, retimed to 75 BPM")
(send mf add-tempo target-tempo)

(define msgs
  (sort
   (append
    (append*
     (for/list ([p (in-list (midi-data-pedals data))])
       (define t (scale-tick (midi-pedal-event-tick p)))
       (list (list t 1 (+ #xB0 (midi-pedal-event-channel p))
                   64 (midi-pedal-event-value p)))))
    (append*
     (for/list ([n (in-list (midi-data-notes data))])
       (define s (scale-tick (midi-note-event-start-tick n)))
       (define e (scale-tick (midi-note-event-end-tick n)))
       (define ch (midi-note-event-channel n))
       (define p (midi-note-event-pitch n))
       (list (list s 2 (+ #x90 ch) p (midi-note-event-velocity n))
             (list e 0 (+ #x80 ch) p 0)))))
   (lambda (a b)
     (or (< (car a) (car b))
         (and (= (car a) (car b)) (< (cadr a) (cadr b)))))))

(define last 0)
(for ([m (in-list msgs)])
  (define tick (car m))
  (send mf add-event (- tick last) (caddr m) (cadddr m) (list-ref m 4))
  (set! last tick))
(send mf save out-midi)
(printf "retimed ~a notes, ~a pedal events -> ~a\n"
        (length (midi-data-notes data))
        (length (midi-data-pedals data))
        out-midi)
