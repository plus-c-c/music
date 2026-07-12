#lang racket

;; ============================================================
;; MIDI 综合测试 — 全部使用 midi/main.rkt 公共 API
;; ============================================================

(require "../midi/main.rkt")

(printf "=== MIDI 综合测试 ===\n\n")

;; ── 1. 基础对象构建 ──

(printf "1. 构建基础对象...\n")

(define notes (list
               (midi-note 60 100 0.5 0)
               (midi-note 62 100 0.5 0)
               (midi-note 64 100 0.5 0)
               (midi-note 65 100 0.5 0)
               (midi-note 67 100 1.0 0)))

(define chords (list
                (midi-chord 60 'major 1.0 100 0)
                (midi-chord 65 'major 1.0 100 0)
                (midi-chord 67 'major 1.0 100 0)))

(define drums (list
               (kick 100 0.5)
               (hi-hat-closed 80 0.25)
               (snare 100 0.5)
               (hi-hat-closed 80 0.25)))

(printf "   音符: ~a, 和弦: ~a, 鼓: ~a\n"
        (length notes) (length chords) (length drums))

;; ── 2. 力度控制 ──

(printf "\n2. 力度控制...\n")

(define cresc-notes (crescendo notes 30 127))
(printf "   渐强: v=~a → v=~a\n"
        (send (car cresc-notes) get-velocity)
        (send (last cresc-notes) get-velocity))

(define soft-notes (apply-dynamic notes 'pp))
(printf "   pp: v=~a\n" (send (car soft-notes) get-velocity))

(define loud-notes (apply-dynamic notes 'ff))
(printf "   ff: v=~a\n" (send (car loud-notes) get-velocity))

;; ── 3. MIDI 文件写入 ──

(printf "\n3. MIDI 文件写入...\n")

(notes->midi-file notes "comprehensive-test.mid")
(printf "   已创建 comprehensive-test.mid\n")

;; ── 4. 实时播放 ──

(printf "\n4. 实时 MIDI 播放...\n")

(define ports (list-midi-out-ports))
(if (null? ports)
    (printf "   无可用端口, 跳过\n")
    (let ([dev (new midi-output%)])
      (send dev open)
      (printf "   端口: ~a\n" (cdr (car ports)))

      ;; 播放音阶
      (printf "   播放音阶...\n")
      (for-each (lambda (n)
                  (send dev note-on (send n get-pitch) (send n get-velocity))
                  (sleep (send n get-duration))
                  (send dev note-off (send n get-pitch)))
                notes)

      ;; 播放和弦
      (printf "   播放和弦...\n")
      (for-each (lambda (c)
                  (for-each (lambda (n)
                              (send dev note-on (send n get-pitch) (send n get-velocity)))
                            (send c get-notes))
                  (sleep (send c get-duration))
                  (for-each (lambda (n)
                              (send dev note-off (send n get-pitch)))
                            (send c get-notes)))
                chords)

      (send dev close)))

;; ── 5. music-stream 播放 ──

(printf "\n5. music-stream 播放...\n")

(if (null? ports)
    (printf "   无可用端口, 跳过\n")
    (let* ([dev (new midi-output%)]
           [_ (send dev open)]
           [melody (new music-stream% [tempo 120])]
           [end0 (send melody start dev
                       (list (midi-note 60 100 0.5 0)
                             (midi-note 62 100 0.5 0)
                             (midi-note 64 100 0.5 0)
                             (midi-note 65 100 0.5 0)
                             (midi-note 67 100 1.0 0)))])
      (printf "   主旋律: 拍 0~a\n" (~r end0 #:precision 1))

      ;; 动态插入
      (send melody wait-until (- end0 1.0))
      (define end1 (send melody insert
                         (crescendo
                          (list (midi-note 72 100 0.5 0)
                                (midi-note 74 100 0.5 0)
                                (midi-note 76 100 0.5 0)
                                (midi-note 77 100 0.5 0)
                                (midi-note 79 100 1.0 0))
                          30 127)
                         #:marker? #f))
      (printf "   插入: 拍 ~a → ~a\n"
              (~r (send melody current-beat) #:precision 1)
              (~r end1 #:precision 1))

      (send melody wait-until end1)
      (sleep 0.3)
      (send melody stop)
      (send dev close)))

;; ── 6. 多流同步 ──

(printf "\n6. 多流同步...\n")

(if (null? ports)
    (printf "   无可用端口, 跳过\n")
    (let* ([dev (new midi-output%)]
           [_ (send dev open)]
           [s1 (new music-stream% [tempo 120])]
           [s2 (new music-stream% [tempo 120] [channel 1])]
           [s1-end (send s1 start dev
                         (list (midi-note 60 100 1.0 0)
                               (midi-note 62 100 1.0 0)
                               (midi-note 64 100 1.0 0)
                               (midi-note 65 100 1.0 0)))])

      ;; s2 从 s1 的第2拍开始同步
      (send s2 start dev
            (list (midi-note 72 100 1.0 1)
                  (midi-note 74 100 1.0 1)
                  (midi-note 76 100 1.0 1))
            #:from s1
            #:at-beat 2.0)

      (printf "   s1: 拍 0~a, s2: 从拍 2 同步\n" (~r s1-end #:precision 1))

      (sleep 3.0)
      (send s1 stop)
      (send s2 stop)
      (send dev close)))

(printf "\n=== 综合测试完成 ===\n")
