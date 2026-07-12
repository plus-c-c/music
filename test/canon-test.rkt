#lang racket

;; ============================================================
;; Pachelbel Canon — 卡农 (正确旋律)
;; ============================================================

(require "../midi/main.rkt")

(define dev (new midi-output%))
(send dev open)
(sleep 0.3)

(define (pause secs) (sleep secs))

;; ============================================================
;; Pachelbel Canon in D Major
;; ============================================================

;; ---- 低音线 (每个音符4拍, 重复2遍) ----
(define bass-line
  (append
   (list (midi-note 38 55 4.0 1)   ; D3
         (midi-note 33 55 4.0 1)   ; A2
         (midi-note 35 55 4.0 1)   ; B2
         (midi-note 30 55 4.0 1)   ; F#2
         (midi-note 31 55 4.0 1)   ; G2
         (midi-note 38 55 4.0 1)   ; D2
         (midi-note 31 55 4.0 1)   ; G2
         (midi-note 33 55 4.0 1))  ; A2
   (list (midi-note 38 55 4.0 1)
         (midi-note 33 55 4.0 1)
         (midi-note 35 55 4.0 1)
         (midi-note 30 55 4.0 1)
         (midi-note 31 55 4.0 1)
         (midi-note 38 55 4.0 1)
         (midi-note 31 55 4.0 1)
         (midi-note 33 55 4.0 1))))

;; ---- 和弦 (每个和弦4拍, 重复2遍) ----
(define chords
  (append
   (list (midi-chord 62 'major 4.0 55 0)   ; D
         (midi-chord 57 'major 4.0 55 0)   ; A
         (midi-chord 59 'minor 4.0 55 0)   ; Bm
         (midi-chord 54 'minor 4.0 55 0)   ; F#m
         (midi-chord 55 'major 4.0 55 0)   ; G
         (midi-chord 62 'major 4.0 55 0)   ; D
         (midi-chord 55 'major 4.0 55 0)   ; G
         (midi-chord 57 'major 4.0 55 0))  ; A
   (list (midi-chord 62 'major 4.0 55 0)
         (midi-chord 57 'major 4.0 55 0)
         (midi-chord 59 'minor 4.0 55 0)
         (midi-chord 54 'minor 4.0 55 0)
         (midi-chord 55 'major 4.0 55 0)
         (midi-chord 62 'major 4.0 55 0)
         (midi-chord 55 'major 4.0 55 0)
         (midi-chord 57 'major 4.0 55 0))))

;; ---- Canon 旋律 (正确版本) ----
;; 每小节4拍, 8小节=32拍, 重复2遍=64拍
(define canon-melody
  (append
   (list
    ;; 小节1 (D): F#5(1) E5(1) D5(1) C#5(1)
    (midi-note 78 80 1.0 0) (midi-note 76 75 1.0 0)
    (midi-note 74 80 1.0 0) (midi-note 73 75 1.0 0)
    ;; 小节2 (A): B4(1) A4(1) B4(1) C#5(1)
    (midi-note 71 80 1.0 0) (midi-note 69 75 1.0 0)
    (midi-note 71 80 1.0 0) (midi-note 73 80 1.0 0)
    ;; 小节3 (Bm): D5(1) C#5(1) B4(1) A4(1)
    (midi-note 74 80 1.0 0) (midi-note 73 75 1.0 0)
    (midi-note 71 80 1.0 0) (midi-note 69 75 1.0 0)
    ;; 小节4 (F#m): G4(1) F#4(1) G4(1) A4(1)
    (midi-note 67 80 1.0 0) (midi-note 66 75 1.0 0)
    (midi-note 67 80 1.0 0) (midi-note 69 80 1.0 0))
   (list
    ;; 小节5 (G): B4(1) A4(1) B4(1) C#5(1)
    (midi-note 71 80 1.0 0) (midi-note 69 75 1.0 0)
    (midi-note 71 80 1.0 0) (midi-note 73 80 1.0 0)
    ;; 小节6 (D): D5(1) C#5(1) B4(1) A4(1)
    (midi-note 74 80 1.0 0) (midi-note 73 75 1.0 0)
    (midi-note 71 80 1.0 0) (midi-note 69 75 1.0 0)
    ;; 小节7 (G): G4(1) F#4(1) E4(1) D4(1)
    (midi-note 67 80 1.0 0) (midi-note 66 75 1.0 0)
    (midi-note 64 80 1.0 0) (midi-note 62 75 1.0 0)
    ;; 小节8 (A): E4(1) D4(1) E4(1) F#4(1)
    (midi-note 64 80 1.0 0) (midi-note 62 75 1.0 0)
    (midi-note 64 80 1.0 0) (midi-note 66 80 1.0 0))))

;; ============================================================
;; 创建 stream
;; ============================================================

(define chord-s (new music-stream% [tempo 72] [channel 0]))
(define bass-s  (new music-stream% [tempo 72] [channel 1]))
(define v1      (new music-stream% [tempo 72] [channel 2]))
(define v2      (new music-stream% [tempo 72] [channel 3]))
(define v3      (new music-stream% [tempo 72] [channel 4]))

;; ---- 启动 ----

(printf "=== Pachelbel Canon in D (tempo=72) ===~n~n")

;; 低音: 基础
(send bass-s start dev bass-line)
(printf "[低音] D-A-Bm-F#m-G-D-G-A ×2 (64拍)~n")

;; 和弦: 同步到低音
(send chord-s start dev chords
      #:from bass-s #:at-beat 0.0)
(printf "[和弦] 同步到低音~n")

;; 声部1: 旋律 (延迟8拍 = 2小节)
(send v1 start dev (append canon-melody canon-melody)
      #:from bass-s #:at-beat 8.0)
(printf "[声部1] 旋律 (延迟2小节)~n")

;; 声部2: 同一旋律 (延迟16拍 = 4小节)
(send v2 start dev (append canon-melody canon-melody)
      #:from bass-s #:at-beat 16.0)
(printf "[声部2] 旋律 (延迟4小节)~n")

;; 声部3: 同一旋律 (延迟24拍 = 6小节)
(send v3 start dev (append canon-melody canon-melody)
      #:from bass-s #:at-beat 24.0)
(printf "[声部3] 旋律 (延迟6小节)~n")

;; ---- 等待播放 ----
(printf "~n播放中...~n")

(for ([t '(16 32 48 64)])
  (pause 8.0)
  (printf "  [拍 ~a]~n" (~r (send bass-s current-beat) #:precision 1)))

(pause 2.0)

;; ---- 停止 ----
(for ([s (list chord-s bass-s v1 v2 v3)])
  (send s stop))

(pause 0.5)
(send dev close)
(printf "~n播放完毕。~n")
