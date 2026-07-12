#lang racket

;; ============================================================
;; 卡农 (Canon) — 多流同步实现
;; ============================================================

(require "../midi/main.rkt")

(define dev (new midi-output%))
(send dev open)
(sleep 0.3)

(define (pause secs) (sleep secs))

;; ---- Pachelbel Canon 和弦进行 (D大调) ----
;; D - A - Bm - F#m - G - D - G - A
(define canon-chords
  (list (midi-chord 62 'major 2.0 80 0)   ; D
        (midi-chord 57 'major 2.0 80 0)   ; A
        (midi-chord 59 'minor 2.0 80 0)   ; Bm
        (midi-chord 54 'minor 2.0 80 0)   ; F#m
        (midi-chord 55 'major 2.0 80 0)   ; G
        (midi-chord 62 'major 2.0 80 0)   ; D
        (midi-chord 55 'major 2.0 80 0)   ; G
        (midi-chord 57 'major 2.0 80 0))) ; A

;; ---- 低音线 ----
(define bass-line
  (list (midi-note 38 70 2.0 1)   ; D
        (midi-note 33 70 2.0 1)   ; A
        (midi-note 35 70 2.0 1)   ; B
        (midi-note 30 70 2.0 1)   ; F#
        (midi-note 31 70 2.0 1)   ; G
        (midi-note 38 70 2.0 1)   ; D
        (midi-note 31 70 2.0 1)   ; G
        (midi-note 33 70 2.0 1))) ; A

;; ---- 旋律 1: 主题 (高音) ----
(define melody1
  (list (midi-note 74 90 1.0 0)   ; D5
        (midi-note 73 85 0.5 0)   ; C#5
        (midi-note 74 90 0.5 0)   ; D5
        (midi-note 76 90 1.0 0)   ; E5
        (midi-note 74 85 0.5 0)   ; D5
        (midi-note 73 85 0.5 0)   ; C#5
        (midi-note 71 90 1.0 0)   ; B4
        (midi-note 69 85 0.5 0)   ; A4
        (midi-note 71 85 0.5 0)   ; B4
        (midi-note 73 90 1.0 0)   ; C#5
        (midi-note 71 85 0.5 0)   ; B4
        (midi-note 69 85 0.5 0)   ; A4
        (midi-note 67 90 1.0 0)   ; G4
        (midi-note 66 85 0.5 0)   ; F#4
        (midi-note 67 85 0.5 0)   ; G4
        (midi-note 69 90 1.0 0)   ; A4
        (midi-note 67 85 0.5 0)   ; G4
        (midi-note 66 85 0.5 0)   ; F#4
        (midi-note 64 90 1.0 0)   ; E4
        (midi-note 62 85 0.5 0)   ; D4
        (midi-note 64 85 0.5 0)   ; E4
        (midi-note 66 90 1.0 0)   ; F#4
        (midi-note 64 85 0.5 0)   ; E4
        (midi-note 62 85 0.5 0))) ; D4

;; ---- 旋律 2: 变奏 (高音) ----
(define melody2
  (list (midi-note 81 85 0.5 0)   ; A5
        (midi-note 79 80 0.5 0)   ; G5
        (midi-note 78 85 0.5 0)   ; F#5
        (midi-note 76 80 0.5 0)   ; E5
        (midi-note 74 85 0.5 0)   ; D5
        (midi-note 73 80 0.5 0)   ; C#5
        (midi-note 74 85 0.5 0)   ; D5
        (midi-note 76 85 0.5 0)   ; E5
        (midi-note 78 85 0.5 0)   ; F#5
        (midi-note 76 80 0.5 0)   ; E5
        (midi-note 74 85 0.5 0)   ; D5
        (midi-note 73 80 0.5 0)   ; C#5
        (midi-note 71 85 0.5 0)   ; B4
        (midi-note 69 80 0.5 0)   ; A4
        (midi-note 71 85 0.5 0)   ; B4
        (midi-note 73 85 0.5 0)   ; C#5
        (midi-note 74 85 0.5 0)   ; D5
        (midi-note 76 80 0.5 0)   ; E5
        (midi-note 78 85 0.5 0)   ; F#5
        (midi-note 76 80 0.5 0)   ; E5
        (midi-note 74 85 0.5 0)   ; D5
        (midi-note 73 80 0.5 0)   ; C#5
        (midi-note 71 85 0.5 0)   ; B4
        (midi-note 69 85 0.5 0))) ; A4

;; ---- 创建 stream ----
(define chord-stream (new music-stream% [tempo 100] [channel 0]))
(define bass-stream  (new music-stream% [tempo 100] [channel 1]))
(define voice1       (new music-stream% [tempo 100] [channel 2]))
(define voice2       (new music-stream% [tempo 100] [channel 3]))
(define voice3       (new music-stream% [tempo 100] [channel 4]))

;; ---- 启动和弦流 (基础) ----
(printf "=== Pachelbel Canon (卡农) ===~n~n")
(define chord-end (send chord-stream start dev
                        (append canon-chords canon-chords)))
(printf "[和弦] D-A-Bm-F#m-G-D-G-A ×2~n")

;; ---- 启动低音流 (同步到和弦) ----
(send bass-stream start dev
      (append bass-line bass-line)
      #:from chord-stream #:at-beat 0.0)
(printf "[低音] 同步到和弦~n")

;; ---- 声部1: 主题 (延迟2拍) ----
(send voice1 start dev melody1
      #:from chord-stream #:at-beat 2.0)
(printf "[声部1] 主题 (延迟2拍)~n")

;; ---- 声部2: 主题重复 (延迟4拍) ----
(send voice2 start dev melody1
      #:from chord-stream #:at-beat 4.0)
(printf "[声部2] 主题重复 (延迟4拍)~n")

;; ---- 声部3: 变奏 (延迟6拍) ----
(send voice3 start dev melody2
      #:from chord-stream #:at-beat 6.0)
(printf "[声部3] 变奏 (延迟6拍)~n")

;; ---- 等待结束 ----
(printf "~n播放中...~n")
(sleep 5.0)
(printf "  [拍 ~a] 声部2加入...~n" (~r (send chord-stream current-beat) #:precision 1))

(sleep 5.0)
(printf "  [拍 ~a] 声部3加入...~n" (~r (send chord-stream current-beat) #:precision 1))

(sleep 15.0)
(printf "  [拍 ~a] 播放结束~n" (~r (send chord-stream current-beat) #:precision 1))

;; ---- 停止 ----
(send chord-stream stop)
(send bass-stream stop)
(send voice1 stop)
(send voice2 stop)
(send voice3 stop)

(pause 0.5)
(send dev close)
(printf "~n卡农播放完毕。~n")
