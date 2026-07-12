#lang racket

;; MIDI Lang 测试文件

(require "midi/main.rkt")

;; 测试 1: 创建基本音符
(printf "=== 测试 1: 创建基本音符 ===\n")
(define note1 (midi-note 60 100 1.0 0))
(printf "音符: ~a\n" (send note1 ->string))

;; 测试 2: 使用音符名称创建音符
(printf "\n=== 测试 2: 使用音符名称创建音符 ===\n")
(define note2 (midi-note-name 'C 4 80 0.5 0))
(printf "音符: ~a\n" (send note2 ->string))

;; 测试 3: 创建和弦
(printf "\n=== 测试 3: 创建和弦 ===\n")
(define chord1 (midi-chord 60 'major 1.0 100 0))
(printf "和弦: ~a\n" (send chord1 ->string))
(printf "和弦音符数: ~a\n" (length (send chord1 get-notes)))

;; 测试 4: 创建打击乐
(printf "\n=== 测试 4: 创建打击乐 ===\n")
(define perc1 (kick 100 0.5))
(printf "打击乐: ~a\n" (send perc1 ->string))

;; 测试 5: 音符变换
(printf "\n=== 测试 5: 音符变换 ===\n")
(define note3 (send note1 transpose 7))
(printf "原始音符: ~a\n" (send note1 ->string))
(printf "移调后: ~a\n" (send note3 ->string))

;; 测试 6: 力度变换
(printf "\n=== 测试 6: 力度变换 ===\n")
(define note4 (send note1 scale-velocity 0.5))
(printf "原始力度: ~a\n" (send note1 get-velocity))
(printf "变换后: ~a\n" (send note4 get-velocity))

;; 测试 7: 创建 MIDI 文件
(printf "\n=== 测试 7: 创建 MIDI 文件 ===\n")
(define file1 (make-midi-file))
(send file1 add-track-name "Test Track")
(send file1 add-tempo 120)
(send file1 add-note 60 100 0)
(send file1 add-note 64 100 0)
(send file1 add-note 67 100 0)
(send file1 save "test-output.mid")
(printf "Created MIDI file: test-output.mid\n")

;; 测试 8: 创建复杂序列
(printf "\n=== 测试 8: 创建复杂序列 ===\n")
(define notes (list
               (midi-note 60 100 0.5 0)
               (midi-note 62 100 0.5 0)
               (midi-note 64 100 0.5 0)
               (midi-note 65 100 0.5 0)
               (midi-note 67 100 1.0 0)))

(for-each (lambda (n) (printf "~a " (send n ->string))) notes)
(newline)

;; 测试 9: 创建并保存和弦进行
(printf "\n=== 测试 9: 创建和弦进行 ===\n")
(define chord-progression
  (list
   (midi-chord 60 'major 1.0 100 0)
   (midi-chord 65 'major 1.0 100 0)
   (midi-chord 67 'major 1.0 100 0)
   (midi-chord 60 'major 1.0 100 0)))

(for-each (lambda (c) (printf "~a " (send c ->string))) chord-progression)
(newline)

;; 测试 10: 创建鼓序列
(printf "\n=== 测试 10: 创建鼓序列 ===\n")
(define drum-sequence
  (list
   (kick 100 0.5)
   (hi-hat-closed 80 0.25)
   (snare 100 0.5)
   (hi-hat-closed 80 0.25)))

(for-each (lambda (d) (printf "~a " (send d ->string))) drum-sequence)
(newline)

;; 测试 11: 保存鼓序列到 MIDI 文件
(printf "\n=== 测试 11: 保存鼓序列 ===\n")
(define drum-file (make-midi-file))
(send drum-file add-track-name "Drum Sequence")
(send drum-file add-tempo 120)
(for-each (lambda (d)
            (let ([events (send d ->events 480)])
              (for-each (lambda (e)
                          (cond
                            [(and (list? e) (= (length e) 3))
                             (send drum-file add-note (cadr e) (caddr e) 0)]
                            [(and (list? e) (= (length e) 2)
                                  (number? (cadr e)))
                             ;; 时值，跳过
                             '()]))
                        events)))
          drum-sequence)
(send drum-file save "drums-output.mid")
(printf "Created drum MIDI file: drums-output.mid\n")

(printf "\n=== 所有测试完成 ===\n")
