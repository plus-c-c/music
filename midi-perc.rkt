#lang racket

(require "midi-core.rkt")

;; MIDI 打击乐类

(define midi-perc%
  (class* object% (equal<%>)
    (super-new)
    (init-field
     [drum 'bass-drum-1]    ; 鼓名称或 MIDI 编号
     [velocity 100]         ; 力度 (0-127)
     [duration 0.5]         ; 时值 (以四分音符为单位)
     [channel 9])           ; MIDI 通道 (打击乐固定为 9)

    ;; 相等性判断
    (define/public (equal-to? other rec)
      (and (is-a? other midi-perc%)
           (equal? drum (send other get-drum))
           (= velocity (send other get-velocity))
           (= duration (send other get-duration))))

    (define/public (equal-hash-code-of rec)
      (bitwise-xor (bitwise-xor velocity duration)
                   (if (symbol? drum)
                       (string-length (symbol->string drum))
                       drum)))

    (define/public (equal-secondary-hash-code-of rec)
      (+ (if (symbol? drum)
             (string-length (symbol->string drum))
             drum)
         (* velocity 1000)
         (* duration 1000000)))

    ;; Getter 方法
    (define/public (get-drum) drum)
    (define/public (get-velocity) velocity)
    (define/public (get-duration) duration)
    (define/public (get-channel) channel)

    ;; 获取 MIDI 编号
    (define/public (get-midi-number)
      (cond
        [(number? drum) drum]
        [(symbol? drum) (hash-ref drum-map drum #f)]
        [else #f]))

    ;; 设置方法
    (define/public (set-drum new-drum)
      (new midi-perc%
           [drum new-drum]
           [velocity velocity]
           [duration duration]
           [channel channel]))

    (define/public (set-velocity new-velocity)
      (new midi-perc%
           [drum drum]
           [velocity (clamp new-velocity 0 127)]
           [duration duration]
           [channel channel]))

    (define/public (set-duration new-duration)
      (new midi-perc%
           [drum drum]
           [velocity velocity]
           [duration (max 0 new-duration)]
           [channel channel]))

    ;; 力度变换
    (define/public (scale-velocity factor)
      (new midi-perc%
           [drum drum]
           [velocity (clamp (round (* velocity factor)) 0 127)]
           [duration duration]
           [channel channel]))

    ;; 时值变换
    (define/public (scale-duration factor)
      (new midi-perc%
           [drum drum]
           [velocity velocity]
           [duration (* duration factor)]
           [channel channel]))

    ;; 转换为 MIDI 事件
    (define/public (->events ticks-per-beat)
      (let* ([midi-num (get-midi-number)]
             [duration-ticks (round (* duration ticks-per-beat))])
        (if midi-num
            (list
             (list (+ NOTE-ON channel) midi-num velocity)
             (list (+ NOTE-OFF channel) midi-num 0)
             duration-ticks)
            '())))

    ;; 转换为字符串表示
    (define/public (->string)
      (format "~a v=~a d=~a" drum velocity duration))

    ;; 打印表示
    (define/public (->print)
      (format "#<midi-perc: ~a>" (->string)))))

;; 辅助函数: 范围限制
(define (clamp val min-val max-val)
  (max min-val (min max-val val)))

;; 便捷构造函数
(define (midi-perc drum [velocity 100] [duration 0.5] [channel 9])
  (new midi-perc%
       [drum drum]
       [velocity velocity]
       [duration duration]
       [channel channel]))

;; 常用打击乐快捷函数
(define (kick [velocity 100] [duration 0.5])
  (midi-perc 'bass-drum-1 velocity duration))

(define (snare [velocity 100] [duration 0.5])
  (midi-perc 'acoustic-snare velocity duration))

(define (hi-hat-closed [velocity 100] [duration 0.25])
  (midi-perc 'closed-hi-hat velocity duration))

(define (hi-hat-open [velocity 100] [duration 0.5])
  (midi-perc 'open-hi-hat velocity duration))

(define (clap [velocity 100] [duration 0.5])
  (midi-perc 'hand-clap velocity duration))

(define (rim [velocity 100] [duration 0.25])
  (midi-perc 'side-stick velocity duration))

(define (tom-high [velocity 100] [duration 0.5])
  (midi-perc 'high-tom velocity duration))

(define (tom-mid [velocity 100] [duration 0.5])
  (midi-perc 'hi-mid-tom velocity duration))

(define (tom-low [velocity 100] [duration 0.5])
  (midi-perc 'low-tom velocity duration))

(define (crash [velocity 100] [duration 1.0])
  (midi-perc 'crash-cymbal-1 velocity duration))

(define (ride [velocity 100] [duration 1.0])
  (midi-perc 'ride-cymbal-1 velocity duration))

(define (cowbell [velocity 100] [duration 0.5])
  (midi-perc 'cowbell velocity duration))

(define (tambourine [velocity 100] [duration 0.5])
  (midi-perc 'tambourine velocity duration))

(provide midi-perc% midi-perc
         kick snare hi-hat-closed hi-hat-open clap rim
         tom-high tom-mid tom-low crash ride cowbell tambourine)
