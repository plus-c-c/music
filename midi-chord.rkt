#lang racket

(require "midi-core.rkt")
(require "midi-note.rkt")

;; MIDI 和弦类

(define midi-chord%
  (class* object% (equal<%>)
    (super-new)
    (init-field
     [root 60]           ; 根音 MIDI 编号
     [type 'major]       ; 和弦类型
     [duration 1.0]      ; 时值 (以四分音符为单位)
     [velocity 100]      ; 力度 (0-127)
     [channel 0])        ; MIDI 通道 (0-15)

    ;; 和弦类型定义 (相对于根音的半音数)
    (define chord-types
      (hash
       'major      '(0 4 7)        ; 大三和弦
       'minor      '(0 3 7)        ; 小三和弦
       'dim        '(0 3 6)        ; 减三和弦
       'aug        '(0 4 8)        ; 增三和弦
       'sus2       '(0 2 7)        ; 挂二和弦
       'sus4       '(0 5 7)        ; 挂四和弦
       'maj7       '(0 4 7 11)     ; 大七和弦
       'min7       '(0 3 7 10)     ; 小七和弦
       'dom7       '(0 4 7 10)     ; 属七和弦
       'dim7       '(0 3 6 9)      ; 减七和弦
       'min7b5     '(0 3 6 10)     ; 半减七和弦
       'aug7       '(0 4 8 10)     ; 增七和弦
       'maj9       '(0 4 7 11 14)  ; 大九和弦
       'min9       '(0 3 7 10 14)  ; 小九和弦
       'dom9       '(0 4 7 10 14)  ; 属九和弦
       'add9       '(0 4 7 14)     ; 加九和弦
       'minadd9    '(0 3 7 14)     ; 小加九和弦
       '6          '(0 4 7 9)      ; 六和弦
       'min6       '(0 3 7 9)      ; 小六和弦
       '7sus4      '(0 5 7 10)     ; 属七挂四和弦
       '9sus4      '(0 5 7 10 14)  ; 属九挂四和弦
       'power      '(0 7)))        ; 强力和弦 (五度)

    ;; 相等性判断
    (define/public (equal-to? other rec)
      (and (is-a? other midi-chord%)
           (= root (send other get-root))
           (eq? type (send other get-type))
           (= duration (send other get-duration))
           (= velocity (send other get-velocity))
           (= channel (send other get-channel))))

    (define/public (equal-hash-code-of rec)
      (bitwise-xor (bitwise-xor (bitwise-xor root velocity) duration) channel))

    (define/public (equal-secondary-hash-code-of rec)
      (+ root (* velocity 128) (* duration 16384) (* channel 2097152)))

    ;; Getter 方法
    (define/public (get-root) root)
    (define/public (get-type) type)
    (define/public (get-duration) duration)
    (define/public (get-velocity) velocity)
    (define/public (get-channel) channel)

    ;; 获取和弦音程
    (define/public (get-intervals)
      (hash-ref chord-types type '(0 4 7)))

    ;; 获取和弦所有音符
    (define/public (get-notes)
      (map (lambda (interval)
             (midi-note (+ root interval) velocity duration channel))
           (get-intervals)))

    ;; 设置方法
    (define/public (set-root new-root)
      (new midi-chord%
           [root (clamp new-root 0 127)]
           [type type]
           [duration duration]
           [velocity velocity]
           [channel channel]))

    (define/public (set-type new-type)
      (new midi-chord%
           [root root]
           [type new-type]
           [duration duration]
           [velocity velocity]
           [channel channel]))

    (define/public (set-duration new-duration)
      (new midi-chord%
           [root root]
           [type type]
           [duration (max 0 new-duration)]
           [velocity velocity]
           [channel channel]))

    (define/public (set-velocity new-velocity)
      (new midi-chord%
           [root root]
           [type type]
           [duration duration]
           [velocity (clamp new-velocity 0 127)]
           [channel channel]))

    (define/public (set-channel new-channel)
      (new midi-chord%
           [root root]
           [type type]
           [duration duration]
           [velocity velocity]
           [channel (clamp new-channel 0 15)]))

    ;; 移调
    (define/public (transpose semitones)
      (new midi-chord%
           [root (clamp (+ root semitones) 0 127)]
           [type type]
           [duration duration]
           [velocity velocity]
           [channel channel]))

    ;; 转换为 MIDI 事件
    (define/public (->events ticks-per-beat)
      (let* ([notes (get-notes)]
             [duration-ticks (round (* duration ticks-per-beat))])
        (append
         ;; 所有音符的 note-on
         (map (lambda (n) (list (+ NOTE-ON channel) (send n get-pitch) velocity)) notes)
         ;; 所有音符的 note-off
         (map (lambda (n) (list (+ NOTE-OFF channel) (send n get-pitch) 0)) notes)
         ;; 时值
         (list duration-ticks))))

    ;; 转换为字符串表示
    (define/public (->string)
      (let ([note-info (midi->note root)])
        (format "~a~a ~a v=~a d=~a ch=~a"
                (car note-info) (cadr note-info) type velocity duration channel)))

    ;; 打印表示
    (define/public (->print)
      (format "#<midi-chord: ~a>" (->string)))))

;; 辅助函数: 范围限制
(define (clamp val min-val max-val)
  (max min-val (min max-val val)))

;; 便捷构造函数
(define (midi-chord root [type 'major] [duration 1.0] [velocity 100] [channel 0])
  (new midi-chord%
       [root root]
       [type type]
       [duration duration]
       [velocity velocity]
       [channel channel]))

;; 从音符名称创建和弦
(define (midi-chord-name name octave [type 'major] [duration 1.0] [velocity 100] [channel 0])
  (midi-chord (note->midi name octave) type duration velocity channel))

(provide midi-chord% midi-chord midi-chord-name)
