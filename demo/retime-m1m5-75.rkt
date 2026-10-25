#lang racket

;; Aria-AMT 对开头 M1–M5 的新识别结果（源文件 120 BPM）
;; 重定时到项目使用的 75 BPM，保留音符和踏板。

(require "../midi-reader.rkt"
         "../midi-file.rkt")

(define in-midi "demo/reference/aria-m1m5-120.mid")
(define out-midi "demo/reference/aria-m1m5-75.mid")
(define target-tempo 75)
(define ppq 480)

(define data (read-midi-data in-midi))
(define (sec->tick s)
  (inexact->exact (round (* s (/ (* ppq target-tempo) 60.0)))))

(define mf (make-midi-file 0 ppq target-tempo))
(send mf add-track-name "Aria-AMT M1-M5, retimed to 75 BPM")
(send mf add-tempo target-tempo)

(define msgs
  (sort
   (append
    (append*
     (for/list ([p (in-list (midi-data-pedals data))])
       (define t (sec->tick (midi-pedal-event-second p)))
       (list (list t 1 (+ #xB0 (midi-pedal-event-channel p))
                   64 (midi-pedal-event-value p)))))
    (append*
     (for/list ([n (in-list (midi-data-notes data))])
       (define s (sec->tick (midi-note-event-start-second n)))
       (define e (sec->tick (midi-note-event-end-second n)))
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
(printf "retimed ~a notes -> ~a\n" (length (midi-data-notes data)) out-midi)
