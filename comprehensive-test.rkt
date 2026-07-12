#lang racket

(require "midi/main.rkt")

(printf "=== 综合测试 ===\n")

;; 创建一个简单的旋律
(define notes (list
               (midi-note 60 100 0.5 0)  ; C4
               (midi-note 62 100 0.5 0)  ; D4
               (midi-note 64 100 0.5 0)  ; E4
               (midi-note 65 100 0.5 0)  ; F4
               (midi-note 67 100 1.0 0))) ; G4

;; 创建和弦
(define chords (list
                (midi-chord 60 'major 1.0 100 0)  ; C major
                (midi-chord 65 'major 1.0 100 0)  ; F major
                (midi-chord 67 'major 1.0 100 0))) ; G major

;; 创建鼓序列
(define drums (list
               (kick 100 0.5)
               (hi-hat-closed 80 0.25)
               (snare 100 0.5)
               (hi-hat-closed 80 0.25)))

;; 写入 MIDI 文件 (使用 notes->midi-file)
(printf "\n1. 写入 MIDI 文件...\n")
(notes->midi-file notes "comprehensive-test.mid")
(printf "   已创建 comprehensive-test.mid\n")

;; 实时播放测试
(printf "\n2. 实时 MIDI 输出测试...\n")
(define ports (list-midi-out-ports))
(if (null? ports)
    (printf "   无可用 MIDI 端口, 跳过实时播放\n")
    (let ([output (new midi-output%)])
      (send output open)
      (printf "   已打开 MIDI 端口\n")
      (printf "   播放音符序列...\n")
      (for-each (lambda (n)
                  (let ([events (send n ->events 480)])
                    (send output play-events events)))
                notes)
      (printf "   播放完成\n")
      (send output close)))

(printf "\n=== 测试完成 ===\n")
