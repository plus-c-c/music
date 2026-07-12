#lang racket

(require "midi-core.rkt")

;; MIDI 音符类

(define midi-note%
  (class* object% (equal<%>)
    (super-new)
    (init-field
     [pitch 60]        ; MIDI 音高 (0-127)
     [velocity 100]    ; 力度 (0-127)
     [duration 1.0]    ; 时值 (以四分音符为单位)
     [channel 0])      ; MIDI 通道 (0-15)

    ;; 相等性判断
    (define/public (equal-to? other rec)
      (and (is-a? other midi-note%)
           (= pitch (send other get-pitch))
           (= velocity (send other get-velocity))
           (= duration (send other get-duration))
           (= channel (send other get-channel))))

    (define/public (equal-hash-code-of rec)
      (bitwise-xor (bitwise-xor (bitwise-xor pitch velocity) duration) channel))

    (define/public (equal-secondary-hash-code-of rec)
      (+ pitch (* velocity 128) (* duration 16384) (* channel 2097152)))

    ;; Getter 方法
    (define/public (get-pitch) pitch)
    (define/public (get-velocity) velocity)
    (define/public (get-duration) duration)
    (define/public (get-channel) channel)

    ;; 设置方法 (返回新对象，保持不可变性)
    (define/public (set-pitch new-pitch)
      (new midi-note%
           [pitch (clamp new-pitch 0 127)]
           [velocity velocity]
           [duration duration]
           [channel channel]))

    (define/public (set-velocity new-velocity)
      (new midi-note%
           [pitch pitch]
           [velocity (clamp new-velocity 0 127)]
           [duration duration]
           [channel channel]))

    (define/public (set-duration new-duration)
      (new midi-note%
           [pitch pitch]
           [velocity velocity]
           [duration (max 0 new-duration)]
           [channel channel]))

    (define/public (set-channel new-channel)
      (new midi-note%
           [pitch pitch]
           [velocity velocity]
           [duration duration]
           [channel (clamp new-channel 0 15)]))

    ;; 音高变换
    (define/public (transpose semitones)
      (new midi-note%
           [pitch (clamp (+ pitch semitones) 0 127)]
           [velocity velocity]
           [duration duration]
           [channel channel]))

    ;; 力度变换
    (define/public (scale-velocity factor)
      (new midi-note%
           [pitch pitch]
           [velocity (clamp (round (* velocity factor)) 0 127)]
           [duration duration]
           [channel channel]))

    ;; 时值变换
    (define/public (scale-duration factor)
      (new midi-note%
           [pitch pitch]
           [velocity velocity]
           [duration (* duration factor)]
           [channel channel]))

    ;; 转换为 MIDI 事件列表 ((status pitch velocity) duration-in-ticks)
    (define/public (->events ticks-per-beat)
      (let ([duration-ticks (round (* duration ticks-per-beat))])
        (list
         (list (+ NOTE-ON channel) pitch velocity)
         (list (+ NOTE-OFF channel) pitch 0)
         duration-ticks)))

    ;; 转换为字符串表示
    (define/public (->string)
      (let* ([note-info (midi->note pitch)]
             [note-name (car note-info)]
             [octave (cadr note-info)])
        (format "~a~a v=~a d=~a ch=~a" note-name octave velocity duration channel)))

    ;; 打印表示
    (define/public (->print)
      (format "#<midi-note: ~a>" (->string)))))

;; 辅助函数: 范围限制
(define (clamp val min-val max-val)
  (max min-val (min max-val val)))

;; 便捷构造函数
(define (midi-note pitch [velocity 100] [duration 1.0] [channel 0])
  (new midi-note%
       [pitch pitch]
       [velocity velocity]
       [duration duration]
       [channel channel]))

;; 从音符名称创建音符
(define (midi-note-name name octave [velocity 100] [duration 1.0] [channel 0])
  (midi-note (note->midi name octave) velocity duration channel))

;; 创建音符序列
(define (midi-sequence notes . args)
  (let ([ticks-per-beat (if (null? args) 480 (car args))])
    (map (lambda (n) (send n ->events ticks-per-beat)) notes)))

(provide midi-note% midi-note midi-note-name midi-sequence)
