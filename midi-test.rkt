#lang racket

;; ============================================================
;; MIDI API 单元测试 — 全部使用 midi/main.rkt 公共 API
;; ============================================================

(require "midi/main.rkt")

(define passed 0)
(define failed 0)

(define (assert v [msg "assertion failed"])
  (unless v (error 'test msg)))

(define (test name thunk)
  (with-handlers ([exn:fail? (lambda (e)
                               (set! failed (+ failed 1))
                               (printf "FAIL ~a: ~a~n" name (exn-message e)))])
    (thunk)
    (set! passed (+ passed 1))
    (printf "PASS ~a~n" name)))

;; ==================== Note ====================

(test "midi-note: 构造与getter"
  (lambda ()
    (define n (midi-note 60 100 1.0 0))
    (assert (= (send n get-pitch) 60))
    (assert (= (send n get-velocity) 100))
    (assert (= (send n get-duration) 1.0))
    (assert (= (send n get-channel) 0))))

(test "midi-note: transpose"
  (lambda ()
    (define n (midi-note 60 100 1.0 0))
    (define n2 (send n transpose 7))
    (assert (= (send n2 get-pitch) 67))))

(test "midi-note: scale-velocity"
  (lambda ()
    (define n (midi-note 60 100 1.0 0))
    (define n2 (send n scale-velocity 0.5))
    (assert (= (send n2 get-velocity) 50))))

(test "midi-note-name: 从名称构造"
  (lambda ()
    (define n (midi-note-name 'C 4 100 1.0 0))
    (assert (= (send n get-pitch) 60))))

(test "midi-note: ->events"
  (lambda ()
    (define n (midi-note 60 100 0.5 0))
    (define evts (send n ->events 480))
    (assert (= (length evts) 3))
    (assert (equal? (car evts) '(144 60 100)))
    (assert (equal? (cadr evts) '(128 60 0)))
    (assert (= (caddr evts) 240))))

;; ==================== Chord ====================

(test "midi-chord: major 和弦音符"
  (lambda ()
    (define c (midi-chord 60 'major 1.0 100 0))
    (define notes (send c get-notes))
    (assert (= (length notes) 3))
    (assert (= (send (list-ref notes 0) get-pitch) 60))
    (assert (= (send (list-ref notes 1) get-pitch) 64))
    (assert (= (send (list-ref notes 2) get-pitch) 67))))

(test "midi-chord: minor 和弦"
  (lambda ()
    (define c (midi-chord 60 'minor 1.0 100 0))
    (define notes (send c get-notes))
    (assert (= (send (list-ref notes 1) get-pitch) 63))))

(test "midi-chord-name: 从名称构造"
  (lambda ()
    (define c (midi-chord-name 'D 4 'minor 1.0 100 0))
    (assert (= (send c get-root) 62))))

;; ==================== Perc ====================

(test "kick: 构造"
  (lambda ()
    (define k (kick 100 0.5))
    (assert (equal? (send k get-drum) 'bass-drum-1))
    (assert (= (send k get-velocity) 100))))

(test "snare: 构造"
  (lambda ()
    (define s (snare 90 0.5))
    (assert (equal? (send s get-drum) 'acoustic-snare))))

(test "midi-perc: 自定义鼓号"
  (lambda ()
    (define p (midi-perc 38 100 0.5))
    (assert (= (send p get-midi-number) 38))))

;; ==================== Dynamics ====================

(test "crescendo: 渐强"
  (lambda ()
    (define notes (list (midi-note 60 10 0.5 0)
                        (midi-note 62 10 0.5 0)
                        (midi-note 64 10 0.5 0)))
    (define result (crescendo notes 30 127))
    (assert (< (send (car result) get-velocity)
               (send (caddr result) get-velocity)))))

(test "decrescendo: 渐弱"
  (lambda ()
    (define notes (list (midi-note 60 10 0.5 0)
                        (midi-note 62 10 0.5 0)
                        (midi-note 64 10 0.5 0)))
    (define result (decrescendo notes 127 30))
    (assert (> (send (car result) get-velocity)
               (send (caddr result) get-velocity)))))

(test "apply-dynamic: 力度标记"
  (lambda ()
    (define notes (list (midi-note 60 10 0.5 0)))
    (define loud (apply-dynamic notes 'ff))
    (assert (= (send (car loud) get-velocity) 127))
    (define soft (apply-dynamic notes 'pp))
    (assert (= (send (car soft) get-velocity) 30))))

(test "constant-velocity: 固定力度"
  (lambda ()
    (define notes (list (midi-note 60 10 0.5 0)
                        (midi-note 62 10 0.5 0)))
    (define result (constant-velocity notes 80))
    (assert (= (send (car result) get-velocity) 80))
    (assert (= (send (cadr result) get-velocity) 80))))

;; ==================== File ====================

(test "make-midi-file: 构造"
  (lambda ()
    (define f (make-midi-file 1 480 120))
    (assert (is-a? f midi-file%))))

(test "notes->midi-file: 写入文件"
  (lambda ()
    (define notes (list (midi-note 60 100 480 0)
                        (midi-note 64 100 480 0)))
    (notes->midi-file notes "/tmp/test-api.mid")
    (assert (file-exists? "/tmp/test-api.mid"))))

;; ==================== Output ====================

(test "midi-output: 列出端口"
  (lambda ()
    (define ports (list-midi-out-ports))
    (assert (list? ports))
    (assert (not (null? ports)))))

(test "midi-output: open/close"
  (lambda ()
    (define out (new midi-output%))
    (send out open)
    (send out close)
    (assert #t)))

;; ==================== Stream ====================

(test "music-stream: 构造与启动"
  (lambda ()
    (define dev (new midi-output%))
    (send dev open)
    (define s (new music-stream% [tempo 120]))
    (define end (send s start dev (list (midi-note 60 100 4.0 0))))
    (assert (> end 0.0))
    (send s stop)
    (send dev close)))

(test "music-stream: insert 小节对齐"
  (lambda ()
    (define dev (new midi-output%))
    (send dev open)
    (define s (new music-stream% [tempo 120] [beats-per-measure 4]))
    (send s start dev (list (midi-note 60 100 8.0 0)))
    (define end1 (send s insert (list (midi-note 72 100 4.0 0)) #:marker? #f))
    (assert (>= end1 8.0))
    (assert (= (modulo (inexact->exact end1) 4) 0))
    (send s stop)
    (send dev close)))

(test "music-stream: 同步 from + at-beat"
  (lambda ()
    (define dev (new midi-output%))
    (send dev open)
    (define s1 (new music-stream% [tempo 120]))
    (define s2 (new music-stream% [tempo 120]))
    (send s1 start dev (list (midi-note 60 100 16.0 0)))
    (send s2 start dev (list (midi-note 72 100 8.0 0))
          #:from s1
          #:at-beat 8.0)
    (sleep 0.5)
    (assert (send s1 is-running?))
    (assert (send s2 is-running?))
    (send s1 stop)
    (send s2 stop)
    (send dev close)))

(test "music-stream: wait-until"
  (lambda ()
    (define dev (new midi-output%))
    (send dev open)
    (define s (new music-stream% [tempo 120]))
    (send s start dev (list (midi-note 60 100 4.0 0)))
    (send s wait-until 2.0)
    (assert (>= (send s current-beat) 1.9))
    (send s stop)
    (send dev close)))

(test "music-stream: set-tempo!"
  (lambda ()
    (define s (new music-stream% [tempo 120]))
    (send s set-tempo! 140)
    (assert (= (send s get-tempo) 140))))

(test "music-stream: append-at"
  (lambda ()
    (define dev (new midi-output%))
    (send dev open)
    (define s (new music-stream% [tempo 120]))
    (send s start dev (list (midi-note 60 100 4.0 0)))
    (define end1 (send s append-at (list (midi-note 64 100 4.0 0))))
    (assert (>= end1 4.0))
    (send s stop)
    (send dev close)))

(test "music-stream: beats-remaining"
  (lambda ()
    (define dev (new midi-output%))
    (send dev open)
    (define s (new music-stream% [tempo 120]))
    (send s start dev (list (midi-note 60 100 4.0 0)))
    (sleep 0.1)
    (define rem (send s beats-remaining))
    (assert (> rem 0.0))
    (send s stop)
    (send dev close)))

(test "music-stream: current-beat"
  (lambda ()
    (define dev (new midi-output%))
    (send dev open)
    (define s (new music-stream% [tempo 120]))
    (send s start dev (list (midi-note 60 100 4.0 0)))
    (sleep 0.5)
    (define beat (send s current-beat))
    (assert (> beat 0.0))
    (send s stop)
    (send dev close)))

;; ==================== Summary ====================

(printf "~n=== 结果: ~a 通过, ~a 失败 ===~n" passed failed)
(when (> failed 0) (exit 1))
