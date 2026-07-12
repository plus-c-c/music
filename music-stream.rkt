#lang racket

;; music-stream: 节拍精确的实时 MIDI 播放流

(require "midi-core.rkt"
         "midi-note.rkt"
         "midi-chord.rkt"
         "midi-perc.rkt"
         "midi-dynamics.rkt"
         "utils/rtmidi.rkt")

(define racket-append append)

(define music-stream%
  (class object%
    (super-new)
    (init-field
     [channel 0]
     [tempo 120]
     [beats-per-measure 4])

    (field [queue '()]
           [queue-lock (make-semaphore 1)]
           [running? #f]
           [start-time 0.0]
           [current-program 0]
           [marker-pitch 96]
           [marker-vel 70]
           [marker-ch 1]
           [legato 1.0])

    (define/public (current-beat)
      (if (= start-time 0.0) 0.0
          (* (- (/ (current-inexact-milliseconds) 1000.0) start-time)
             (/ tempo 60.0))))

    (define/public (beat->sec b) (* b (/ 60.0 tempo)))

    (define (measure-align b)
      (* beats-per-measure (ceiling (/ b beats-per-measure))))

    (define (queue-event! beat event)
      (semaphore-wait queue-lock)
      (set! queue (cons (cons beat event) queue))
      (semaphore-post queue-lock))

    (define (dequeue-before! now-beat)
      (semaphore-wait queue-lock)
      (define ready '())
      (define remaining '())
      (for ([ev queue])
        (if (<= (car ev) now-beat)
            (set! ready (cons ev ready))
            (set! remaining (cons ev remaining))))
      (set! queue remaining)
      (semaphore-post queue-lock)
      (map cdr ready))

    (define (last-queued-beat)
      (if (null? queue) 0.0 (apply max (map car queue))))

    (define (queue-note! t note)
      (define dur (send note get-duration))
      (define off (+ t (* dur legato)))
      (cond
        [(is-a? note midi-chord%)
         (define vel (send note get-velocity))
         (define ch (send note get-channel))
         (for ([n (send note get-notes)])
           (queue-event! t (list (+ NOTE-ON ch) (send n get-pitch) vel))
           (queue-event! off (list (+ NOTE-OFF ch) (send n get-pitch) 0)))]
        [(is-a? note midi-perc%)
         (define mn (send note get-midi-number))
         (define vel (send note get-velocity))
         (when mn
           (queue-event! t (list (+ NOTE-ON 9) mn vel))
           (queue-event! off (list (+ NOTE-OFF 9) mn 0)))]
        [else
         (define vel (send note get-velocity))
         (define ch (send note get-channel))
         (queue-event! t (list (+ NOTE-ON ch) (send note get-pitch) vel))
         (queue-event! off (list (+ NOTE-OFF ch) (send note get-pitch) 0))])
      (+ t dur))

    (define (queue-notes! start-beat notes)
      (define t start-beat)
      (for ([note notes])
        (set! t (queue-note! t note)))
      t)

    (define (play-loop dev)
      (let loop ()
        (when running?
          (define events (dequeue-before! (+ (current-beat) 0.1)))
          (for ([ev events])
            (when (list? ev)
              (send dev send-message ev)))
          (sleep 0.001)
          (loop))))

    ;; start: 启动播放
    ;;   #:from   - 从已有 music-stream 同步 (共享时间基准)
    ;;   #:at-beat - 在指定拍点开始排列 (配合 #:from 使用)
    (define/public (start dev initial-notes
                          #:from [source #f]
                          #:at-beat [beat 0.0])
      (set! queue '())
      (cond
        ;; 从已有 stream 同步
        [source
         (define src-beat (send source current-beat))
         (define align-beat (max beat (measure-align src-beat)))
         (set! start-time (- (/ (current-inexact-milliseconds) 1000.0)
                            (* src-beat (/ 60.0 tempo))))
         (queue-notes! align-beat initial-notes)]
        ;; 独立启动
        [else
         (set! start-time 0.0)
         (queue-notes! 0.0 initial-notes)
         (set! start-time (+ (/ (current-inexact-milliseconds) 1000.0) 0.1))])
      (set! running? #t)
      (thread (lambda () (play-loop dev)))
      (last-queued-beat))

    (define/public (stop)
      (set! running? #f)
      (sleep 0.05)
      (semaphore-wait queue-lock)
      (set! queue '())
      (semaphore-post queue-lock))

    (define/public (wait-until beat)
      (let loop ()
        (when (and running? (< (current-beat) beat))
          (sleep 0.01)
          (loop))))

    (define/public (insert notes #:marker? [marker? #t])
      (define insert-at (measure-align (last-queued-beat)))
      (define content (if marker? (with-markers notes) notes))
      (define end-beat (queue-notes! insert-at content))
      (measure-align end-beat))

    (define/public (append-at notes)
      (define insert-at (max (+ (last-queued-beat) 0.01)
                            (measure-align (last-queued-beat))))
      (define end-beat (queue-notes! insert-at notes))
      (measure-align end-beat))

    (define/public (set-program dev prog [ch channel])
      (set! current-program prog)
      (send dev pc prog ch))

    (define (with-markers notes)
      (cons (midi-note marker-pitch marker-vel 0.0 marker-ch)
            (racket-append notes
                    (list (midi-note marker-pitch marker-vel 0.0 marker-ch)))))

    (define/public (get-tempo) tempo)
    (define/public (set-tempo! t) (set! tempo t))
    (define/public (is-running?) running?)
    (define/public (beats-remaining)
      (max 0 (- (last-queued-beat) (current-beat))))))

(provide music-stream%)
