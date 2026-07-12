#lang racket

;; ============================================================
;; Pachelbel Canon — 卡农 (完整旋律)
;; ============================================================

(require "../midi/main.rkt")

(define dev (new midi-output%))
(send dev open)
(sleep 0.3)

(define (pause secs) (sleep secs))

;; ============================================================
;; Pachelbel Canon in D Major (tempo=80)
;;
;; 旋律来源: Arduino Pachelbel Canon (Robson Couto)
;; 时值: 2=二分音符(2拍), 4=四分音符(1拍), 8=八分音符(0.5拍)
;; ============================================================

;; ---- 低音线 (8音×4拍×2遍=64拍) ----
(define bass-line
  (append
   (list (midi-note 38 55 4.0 1) (midi-note 33 55 4.0 1)
         (midi-note 35 55 4.0 1) (midi-note 30 55 4.0 1)
         (midi-note 31 55 4.0 1) (midi-note 38 55 4.0 1)
         (midi-note 31 55 4.0 1) (midi-note 33 55 4.0 1))
   (list (midi-note 38 55 4.0 1) (midi-note 33 55 4.0 1)
         (midi-note 35 55 4.0 1) (midi-note 30 55 4.0 1)
         (midi-note 31 55 4.0 1) (midi-note 38 55 4.0 1)
         (midi-note 31 55 4.0 1) (midi-note 33 55 4.0 1))))

;; ---- 和弦 (8和弦×4拍×2遍=64拍) ----
(define chords
  (append
   (list (midi-chord 62 'major 4.0 50 0) (midi-chord 57 'major 4.0 50 0)
         (midi-chord 59 'minor 4.0 50 0) (midi-chord 54 'minor 4.0 50 0)
         (midi-chord 55 'major 4.0 50 0) (midi-chord 62 'major 4.0 50 0)
         (midi-chord 55 'major 4.0 50 0) (midi-chord 57 'major 4.0 50 0))
   (list (midi-chord 62 'major 4.0 50 0) (midi-chord 57 'major 4.0 50 0)
         (midi-chord 59 'minor 4.0 50 0) (midi-chord 54 'minor 4.0 50 0)
         (midi-chord 55 'major 4.0 50 0) (midi-chord 62 'major 4.0 50 0)
         (midi-chord 55 'major 4.0 50 0) (midi-chord 57 'major 4.0 50 0))))

;; ---- Canon 完整旋律 ----
;; 来自 Arduino Pachelbel Canon, 转换为 MIDI note + duration
;; 时值转换: 2→2.0, 4→1.0, 8→0.5
(define canon-melody
  (list
   ;; === 第一段: 简单四分音符 (24拍) ===
   ;; F#4 E4 D4 C#4 | B3 A3 B3 C#4
   (midi-note 66 80 1.0 0) (midi-note 64 75 1.0 0)
   (midi-note 62 80 1.0 0) (midi-note 61 75 1.0 0)
   (midi-note 59 80 1.0 0) (midi-note 57 75 1.0 0)
   (midi-note 59 80 1.0 0) (midi-note 61 80 1.0 0)
   ;; F#4 E4 D4 C#4 | B3 A3 B3 C#4
   (midi-note 66 80 1.0 0) (midi-note 64 75 1.0 0)
   (midi-note 62 80 1.0 0) (midi-note 61 75 1.0 0)
   (midi-note 59 80 1.0 0) (midi-note 57 75 1.0 0)
   (midi-note 59 80 1.0 0) (midi-note 61 80 1.0 0)
   ;; D4 C#4 B3 A3 | G3 F#3 G3 A3
   (midi-note 62 80 1.0 0) (midi-note 61 75 1.0 0)
   (midi-note 59 80 1.0 0) (midi-note 57 75 1.0 0)
   (midi-note 55 80 1.0 0) (midi-note 54 75 1.0 0)
   (midi-note 55 80 1.0 0) (midi-note 57 80 1.0 0)

   ;; === 第二段: 变奏 (24拍) ===
   ;; D4(1) F#4(0.5) G4(0.5) A4(1) F#4(0.5) G4(0.5)
   (midi-note 62 80 1.0 0) (midi-note 66 75 0.5 0)
   (midi-note 67 80 0.5 0) (midi-note 69 80 1.0 0)
   (midi-note 66 75 0.5 0) (midi-note 67 80 0.5 0)
   ;; A4(1) B3(0.5) C#4(0.5) D4(0.5) E4(0.5) F#4(0.5) G4(0.5)
   (midi-note 69 80 1.0 0) (midi-note 59 75 0.5 0)
   (midi-note 61 80 0.5 0) (midi-note 62 80 0.5 0)
   (midi-note 64 75 0.5 0) (midi-note 66 80 0.5 0)
   (midi-note 67 80 0.5 0)
   ;; F#4(1) D4(0.5) E4(0.5) F#4(1) F#3(0.5) G3(0.5)
   (midi-note 66 80 1.0 0) (midi-note 62 75 0.5 0)
   (midi-note 64 80 0.5 0) (midi-note 66 80 1.0 0)
   (midi-note 54 75 0.5 0) (midi-note 55 80 0.5 0)
   ;; A3(0.5) G3(0.5) F#3(0.5) G3(0.5) A3(2)
   (midi-note 57 80 0.5 0) (midi-note 55 75 0.5 0)
   (midi-note 54 80 0.5 0) (midi-note 55 75 0.5 0)
   (midi-note 57 80 2.0 0)
   ;; G3(1) B3(0.5) A3(0.5) G3(1) F#3(0.5) E3(0.5)
   (midi-note 55 80 1.0 0) (midi-note 59 75 0.5 0)
   (midi-note 57 80 0.5 0) (midi-note 55 80 1.0 0)
   (midi-note 54 75 0.5 0) (midi-note 52 80 0.5 0)
   ;; F#3(1) D3(0.5) E3(0.5) F#3(0.5) G3(0.5) A3(0.5) B3(0.5)
   (midi-note 54 80 1.0 0) (midi-note 50 75 0.5 0)
   (midi-note 52 80 0.5 0) (midi-note 54 80 0.5 0)
   (midi-note 55 75 0.5 0) (midi-note 57 80 0.5 0)
   (midi-note 59 80 0.5 0)

   ;; === 第三段: 高潮 (16拍) ===
   ;; G3(1) B3(0.5) A3(0.5) B3(1) C#4(0.5) D4(0.5)
   (midi-note 55 80 1.0 0) (midi-note 59 75 0.5 0)
   (midi-note 57 80 0.5 0) (midi-note 59 80 1.0 0)
   (midi-note 61 75 0.5 0) (midi-note 62 80 0.5 0)
   ;; A3(0.5) B3(0.5) C#4(0.5) D4(0.5) E4(0.5) F#4(0.5) G4(0.5) A4(2)
   (midi-note 57 80 0.5 0) (midi-note 59 75 0.5 0)
   (midi-note 61 80 0.5 0) (midi-note 62 75 0.5 0)
   (midi-note 64 80 0.5 0) (midi-note 66 75 0.5 0)
   (midi-note 67 80 0.5 0) (midi-note 69 80 2.0 0)
   ;; A4(1) F#4(0.5) G4(0.5) A4(1)
   (midi-note 69 80 1.0 0) (midi-note 66 75 0.5 0)
   (midi-note 67 80 0.5 0) (midi-note 69 80 1.0 0)
   ;; F#4(0.5) G4(0.5) A4(0.5) A3(0.5) B3(0.5) C#4(0.5)
   (midi-note 66 75 0.5 0) (midi-note 67 80 0.5 0)
   (midi-note 69 80 0.5 0) (midi-note 57 75 0.5 0)
   (midi-note 59 80 0.5 0) (midi-note 61 80 0.5 0)
   ;; D4(0.5) E4(0.5) F#4(0.5) G4(0.5) F#4(1) D4(0.5) E4(0.5)
   (midi-note 62 80 0.5 0) (midi-note 64 75 0.5 0)
   (midi-note 66 80 0.5 0) (midi-note 67 75 0.5 0)
   (midi-note 66 80 1.0 0) (midi-note 62 75 0.5 0)
   (midi-note 64 80 0.5 0)
   ;; F#4(0.5) C#4(0.5) A3(0.5) A3(0.5)
   (midi-note 66 80 0.5 0) (midi-note 61 75 0.5 0)
   (midi-note 57 80 0.5 0) (midi-note 57 80 0.5 0)))

;; ============================================================
;; 创建 stream
;; ============================================================

(define chord-s (new music-stream% [tempo 80] [channel 0]))
(define bass-s  (new music-stream% [tempo 80] [channel 1]))
(define v1      (new music-stream% [tempo 80] [channel 2]))
(define v2      (new music-stream% [tempo 80] [channel 3]))
(define v3      (new music-stream% [tempo 80] [channel 4]))

;; ---- 启动 ----

(printf "=== Pachelbel Canon in D (tempo=80) ===~n~n")

;; 低音: 基础
(send bass-s start dev bass-line)
(printf "[低音] D-A-Bm-F#m-G-D-G-A ×2~n")

;; 和弦: 同步
(send chord-s start dev chords
      #:from bass-s #:at-beat 0.0)
(printf "[和弦] 同步到低音~n")

;; 声部1: 旋律 (延迟8拍 = 2小节)
(send v1 start dev canon-melody
      #:from bass-s #:at-beat 8.0)
(printf "[声部1] 旋律 (延迟2小节)~n")

;; 声部2: 同一旋律 (延迟16拍 = 4小节)
(send v2 start dev canon-melody
      #:from bass-s #:at-beat 16.0)
(printf "[声部2] 旋律 (延迟4小节)~n")

;; 声部3: 同一旋律 (延迟24拍 = 6小节)
(send v3 start dev canon-melody
      #:from bass-s #:at-beat 24.0)
(printf "[声部3] 旋律 (延迟6小节)~n")

;; ---- 等待播放 ----
(printf "~n播放中...~n")

(for ([t '(16 32 48 64)])
  (pause 10.0)
  (printf "  [拍 ~a]~n" (~r (send bass-s current-beat) #:precision 1)))

(pause 2.0)

;; ---- 停止 ----
(for ([s (list chord-s bass-s v1 v2 v3)])
  (send s stop))

(pause 0.5)
(send dev close)
(printf "~n播放完毕。~n")
