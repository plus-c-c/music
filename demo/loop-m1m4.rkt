#lang racket

;; 将当前 M1–M4 循环播放指定次数。
;;
;; 具体循环规则由仓库库 midi-loop.rkt 提供：
;;   - start 段 / end 段分别接受独立的修饰方法；
;;   - 第一遍保留 M1 前的弱起；
;;   - 循环时默认排除 Melody 声部；
;;   - 后续遍跳过 M1 原始弱起；
;;   - 中间遍用连续踏板把 M4 末尾 3/16 延入下一遍 M1；
;;   - 最后一遍 M4 末尾 3/16 的音符为休止，踏板延长到小节结尾。

(require "../midi/main.rkt"
         "kaitou-aria-intro.rkt"
         racket/cmdline)

(provide loop-m1-m4)

(define cycle-start bar-offset)       ; M1 小节线 1.25
(define cycle-length 16)              ; 4 小节
(define pickup-window 3/4)            ; M1 弱起窗口
(define m4-end-length 3/4)            ; M4 最后 3/16 全音符 = 0.75 拍

(define (events-end events)
  (for/fold ([m 0]) ([e (in-list events)])
    (max m (+ (midi-event-start e) (midi-event-duration e)))))

(define (pedals-end pedals)
  (for/fold ([m 0]) ([p (in-list pedals)])
    (max m (+ (pedal-event-start p) (pedal-event-duration p)))))

;; 结束段单独修饰：音符保持休止，踏板覆盖整个结束段。
(define (end-section-pedal-to-bar segment)
  (loop-segment-hold-pedal segment))

(define (loop-m1-m4 n [play? #t]
                    #:include-melody? [include-melody? #f]
                    #:start-transform [start-transform (lambda (segment) segment)]
                    #:end-transform [end-transform end-section-pedal-to-bar])
  (define excluded-voices (if include-melody? '() '("Melody")))
  (define active-voices
    (if include-melody?
        voices
        (filter (lambda (v) (not (equal? (voice-name v) "Melody")))
                voices)))
  (define-values (events pedals)
    (loop-score piece n
                #:start cycle-start
                #:length cycle-length
                #:end-length m4-end-length
                #:pickup-window pickup-window
                #:start-transform start-transform
                #:end-transform end-transform
                #:exclude-voices excluded-voices
                #:bridge-pedals? #t))
  (printf "loop ~a times: events ~a, pedals ~a, end ~a beats, pedal-end ~a beats\n"
          n (length events) (length pedals)
          (events-end events) (pedals-end pedals))
  (when play?
    (play-events events (length active-voices)
                 #:tempo tempo
                 #:backend 'audio
                 #:pedals pedals))
  (values events pedals))

(module+ main
  (define no-play? #f)
  (command-line
   #:program "loop-m1-m4"
   #:once-each
   [("--no-play") "只生成循环事件，不播放" (set! no-play? #t)]
   #:args (n)
   (begin
     (loop-m1-m4 (string->number n) (not no-play?))
     (void))))
