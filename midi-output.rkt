#lang racket

(require "midi-core.rkt"
         "utils/rtmidi.rkt")

;; MIDI 实时输出模块 (基于本地 rtmidi FFI wrapper)

;; 查找合成器端口 (优先 FluidSynth, 否则第一个非Midi Through端口)
(define (find-synth-port)
  (define ports (list-midi-out-ports))
  (define (synth? name)
    (regexp-match #rx"[Ff][Ll][Uu][Ii][Dd]|[Ss][Yy][Nn][Tt][Hh]|[Gg][Mm]|[Gg][Ss]|[Xx][Gg]" name))
  (define (through? name)
    (regexp-match #rx"[Mm]idi [Tt]hrough" name))
  (cond
    [(for/first ([p ports] #:when (synth? (cdr p))) (car p))]
    [(for/first ([p ports] #:when (not (through? (cdr p)))) (car p))]
    [else 0]))

(define midi-output%
  (class object%
    (super-new)
    (init-field
     [channel 0]
     [port-idx #f]    ; #f = 自动检测合成器端口
     [device-name "Music DSL"])

    (field [rtmidi-out #f]
           [is-open? #f])

    (define/public (open)
      (with-handlers ([exn:fail? (lambda (e)
                                   (set! is-open? #f)
                                   (error 'midi-open
                                          "无法打开 MIDI 输出: ~a"
                                          (exn-message e)))])
        (set! rtmidi-out (new rtmidi-out%))
        (define idx (if port-idx port-idx (find-synth-port)))
        (send rtmidi-out open idx device-name)
        (set! is-open? #t)
        (printf "已打开 MIDI 端口: ~a~n"
                (cdr (list-ref (send rtmidi-out list-ports) idx)))))

    (define/public (close)
      (when (and rtmidi-out is-open?)
        (send rtmidi-out close)
        (set! is-open? #f)
        (set! rtmidi-out #f)
        (printf "已关闭 MIDI 端口~n")))

    (define/public (send-message msg)
      (unless is-open?
        (error 'midi-send "MIDI 输出未打开"))
      (send rtmidi-out send-message msg))

    (define/public (note-on pitch [velocity 100] [ch channel])
      (send-message (list (+ NOTE-ON ch) (clamp pitch 0 127) (clamp velocity 0 127))))

    (define/public (note-off pitch [velocity 0] [ch channel])
      (send-message (list (+ NOTE-OFF ch) (clamp pitch 0 127) (clamp velocity 0 127))))

    (define/public (control-change controller value [ch channel])
      (send-message (list (+ CONTROL-CHANGE ch)
                          (clamp controller 0 127)
                          (clamp value 0 127))))

    (define/public (program-change program [ch channel])
      (send-message (list (+ PROGRAM-CHANGE ch) (clamp program 0 127))))

    (define/public (pitch-bend value [ch channel])
      (let ([clamped (clamp value 0 16383)])
        (send-message (list (+ PITCH-BEND ch)
                            (bitwise-and clamped #x7F)
                            (arithmetic-shift clamped -7)))))

    (define/public (play-note pitch [velocity 100] [duration 0.5] [ch channel])
      (note-on pitch velocity ch)
      (sleep duration)
      (note-off pitch 0 ch))

    (define/public (play-chord pitches [velocity 100] [duration 0.5] [ch channel])
      (for-each (lambda (p) (note-on p velocity ch)) pitches)
      (sleep duration)
      (for-each (lambda (p) (note-off p 0 ch)) pitches))

    (define/public (play-events events [ticks-per-beat 480] [tempo 120])
      (let ([seconds-per-tick (/ 60.0 (* tempo ticks-per-beat))])
        (for-each (lambda (event)
                    (cond
                      [(and (list? event) (= (length event) 3))
                       (send-message event)]
                      [(and (list? event) (= (length event) 2))
                       (sleep (* (cadr event) seconds-per-tick))]
                      [(number? event)
                       (sleep (* event seconds-per-tick))]))
                  events)))

    (define/public (list-ports)
      (list-midi-out-ports))))

;; 辅助函数
(define (clamp val min-val max-val)
  (max min-val (min max-val val)))

;; 便捷构造函数
(define (make-midi-output [ch 0] [port #f])
  (new midi-output% [channel ch] [port-idx port]))

;; 列出端口
(define (open-midi-ports)
  (let ([ports (list-midi-out-ports)])
    (printf "可用 MIDI 输出端口:~n")
    (for-each (lambda (p) (printf "  ~a: ~a~n" (car p) (cdr p)))
              ports)
    ports))

(provide midi-output% make-midi-output open-midi-ports)
