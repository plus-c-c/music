#lang racket

;; ============================================================
;; NEST — Main Theme (PV scoring sketch)
;;
;; 用途：ZJU-China 2026 iGEM PV 配乐主旋律示意
;; 调性：D major，76 BPM，4/4
;; 结构：Intro 2 bars → A 8 bars → B 8 bars → C 7 bars
;;
;; 动机：
;;   D–F#–A–E (add9) 的“修复/生长”色彩
;;   旋律以 F#–E–D–A 的下行三音为核心，B 段上行模进，
;;   C 段回到高八度并落在 D 主和弦。
;;
;; 运行：
;;   racket demo/nest-theme.rkt
;; 需要先启动 FluidSynth（ALSA sequencer）：
;;   fluidsynth -a pulseaudio -m alsa_seq -g 0.5 \
;;     /usr/share/soundfonts/FluidR3_GM.sf2
;; ============================================================

(require "../midi/main.rkt")

(provide tempo
         pad-notes bass-notes melody-notes arp-notes
         string-notes bell-notes bell-final-notes
         kick-notes cymbal-notes cymbal-final-notes)

(define tempo 76)
(define (n pitch dur vel ch) (midi-note pitch vel dur ch))
(define (c root type dur vel ch) (midi-chord root type dur vel ch))
(define (p drum dur vel) (midi-perc drum vel dur))

;; ------------------------------------------------------------
;; 和声：每小节一个和弦
;; ------------------------------------------------------------

;; Pad (ch 2, Warm Pad)：2 小节 Intro + 22 小节
(define pad-notes
  (list
   ;; Intro: Dadd9, 8 beats
   (c 50 'add9 8 52 2)
   ;; A: D – A – Bm – G – D – A – G – D
   (c 50 'add9 4 50 2) (c 57 'major 4 48 2)
   (c 59 'minor 4 48 2) (c 55 'major 4 50 2)
   (c 50 'add9 4 50 2) (c 57 'major 4 48 2)
   (c 55 'major 4 50 2) (c 50 'add9 4 50 2)
   ;; B: Bm – G – D – A – Bm – G – Em7 – A
   (c 59 'minor 4 52 2) (c 55 'major 4 52 2)
   (c 50 'add9 4 54 2) (c 57 'major 4 54 2)
   (c 59 'minor 4 56 2) (c 55 'major 4 58 2)
   (c 52 'min7 4 58 2) (c 57 'major 4 60 2)
   ;; C: D – A – Bm – G – A – D (last 8 beats)
   (c 50 'add9 4 60 2) (c 57 'major 4 60 2)
   (c 59 'minor 4 62 2) (c 55 'major 4 64 2)
   (c 57 'major 4 66 2) (c 50 'major 12 64 2)))

;; Bass (ch 3, Contrabass)
(define bass-notes
  (list
   ;; A: root / fifth, half notes
   (n 38 2 70 3) (n 45 2 62 3)
   (n 33 2 70 3) (n 40 2 62 3)
   (n 35 2 70 3) (n 42 2 62 3)
   (n 31 2 70 3) (n 38 2 62 3)
   (n 38 2 72 3) (n 45 2 64 3)
   (n 33 2 72 3) (n 40 2 64 3)
   (n 31 2 72 3) (n 38 2 64 3)
   (n 38 2 72 3) (n 45 2 64 3)
   ;; B: quarter pulse
   (n 35 1 78 3) (n 35 1 62 3) (n 42 1 72 3) (n 35 1 62 3)
   (n 31 1 78 3) (n 31 1 62 3) (n 38 1 72 3) (n 31 1 62 3)
   (n 38 1 78 3) (n 38 1 62 3) (n 45 1 72 3) (n 38 1 62 3)
   (n 33 1 78 3) (n 33 1 62 3) (n 40 1 72 3) (n 33 1 62 3)
   (n 35 1 80 3) (n 35 1 64 3) (n 42 1 74 3) (n 35 1 64 3)
   (n 31 1 80 3) (n 31 1 64 3) (n 38 1 74 3) (n 31 1 64 3)
   (n 40 1 80 3) (n 40 1 64 3) (n 47 1 74 3) (n 40 1 64 3)
   (n 33 1 80 3) (n 33 1 64 3) (n 40 1 74 3) (n 33 1 64 3)
   ;; C: sustained roots
   (n 38 4 82 3) (n 33 4 80 3) (n 35 4 80 3)
   (n 31 4 82 3) (n 33 4 84 3) (n 38 12 86 3)))

;; Melody (ch 0, Piano)
(define melody-notes
  (list
   ;; A — 8 bars
   (n 66 1.5 90 0) (n 64 0.5 82 0) (n 62 1 88 0) (n 57 1 78 0)
   (n 61 1.5 88 0) (n 62 0.5 82 0) (n 64 2 92 0)
   (n 66 1 88 0) (n 69 1 92 0) (n 71 1.5 100 0) (n 69 0.5 92 0)
   (n 67 2 94 0) (n 66 1 88 0) (n 64 1 84 0)
   (n 66 1.5 90 0) (n 64 0.5 82 0) (n 62 1 88 0) (n 57 1 78 0)
   (n 61 1.5 90 0) (n 62 0.5 84 0) (n 64 1 90 0) (n 61 1 84 0)
   (n 62 1 86 0) (n 64 1 88 0) (n 66 1 92 0) (n 67 1 94 0)
   (n 69 2 98 0) (n 66 2 90 0)
   ;; B — 8 bars
   (n 71 1.5 96 0) (n 69 0.5 88 0) (n 66 1 92 0) (n 62 1 84 0)
   (n 67 1.5 92 0) (n 69 0.5 86 0) (n 71 2 98 0)
   (n 69 1.5 94 0) (n 71 0.5 88 0) (n 74 2 102 0)
   (n 73 2 100 0) (n 71 1 94 0) (n 69 1 90 0)
   (n 66 1 90 0) (n 71 1 94 0) (n 74 1.5 102 0) (n 73 0.5 94 0)
   (n 71 2 98 0) (n 69 1 92 0) (n 67 1 88 0)
   (n 69 1.5 94 0) (n 67 0.5 88 0) (n 66 2 96 0)
   (n 64 2 92 0) (n 61 2 86 0)
   ;; C — 7 bars (last bar rings 8 beats)
   (n 74 1.5 102 0) (n 73 0.5 94 0) (n 71 1 98 0) (n 69 1 92 0)
   (n 73 2 104 0) (n 71 1 98 0) (n 69 1 94 0)
   (n 71 1.5 100 0) (n 69 0.5 92 0) (n 66 2 98 0)
   (n 67 1 94 0) (n 69 1 96 0) (n 71 1 98 0) (n 74 1 104 0)
   (n 73 2 104 0) (n 76 2 106 0)
   (n 74 12 100 0)))

;; Harp arpeggios (ch 4): A + B + first 5 bars of C, then final chord
(define (arp root third)
  (for/list ([p (list root (+ root 7) (+ root third) (+ root 7)
                      (+ root 12) (+ root 7) (+ root third) (+ root 7))])
    (n p 0.5 48 4)))

(define arp-progression
  (list (list 50 4) (list 45 4) (list 47 3) (list 43 4)
        (list 50 4) (list 45 4) (list 43 4) (list 50 4)
        (list 47 3) (list 43 4) (list 50 4) (list 45 4)
        (list 47 3) (list 43 4) (list 52 3) (list 45 4)
        (list 50 4) (list 45 4) (list 47 3) (list 43 4)
        (list 45 4)))

(define arp-notes
  (append
   (append* (for/list ([pair arp-progression])
              (arp (car pair) (cadr pair))))
   (list (c 62 'major 12 56 4))))

;; Strings (ch 1): enter at B, then bloom into chords at C
(define string-notes
  (list
   ;; B: counter-line
   (n 66 4 58 1) (n 67 4 60 1) (n 69 4 62 1) (n 73 4 66 1)
   (n 74 4 68 1) (n 71 4 64 1) (n 67 4 60 1) (n 64 4 58 1)
   ;; C: sustained chord pads
   (c 62 'major 4 68 1) (c 57 'major 4 66 1)
   (c 59 'minor 4 68 1) (c 55 'major 4 70 1)
   (c 57 'major 4 72 1) (c 62 'major 12 76 1)))

;; Bells (ch 5): NEST reveal sparkle, then final tag
(define bell-notes
  (list (n 86 8 44 5)))          ; D6 at C
(define bell-final-notes
  (list (n 86 12 52 5)))         ; final D6

;; Kick (ch 9): heartbeat in B, pulse in C
(define (kick-2 vel) (p 'bass-drum-1 2 vel))
(define (kick-1 vel) (p 'bass-drum-1 1 vel))
(define kick-notes
  (append
   (append* (for/list ([bar (in-range 8)])
              (list (kick-2 54) (kick-2 48))))
   (append* (for/list ([bar (in-range 5)])
              (list (kick-1 58) (kick-1 48) (kick-1 54) (kick-1 48))))
   (list (p 'bass-drum-1 12 64))))

;; Cymbal (ch 9): C section lift and final ring
(define cymbal-notes
  (list (p 'crash-cymbal-1 8 42)))
(define cymbal-final-notes
  (list (p 'crash-cymbal-1 12 54)))

;; ------------------------------------------------------------
;; Live playback (only when this file is run directly)
;; ------------------------------------------------------------
(module+ main
(define pad    (new music-stream% [tempo tempo] [channel 2]))
(define piano  (new music-stream% [tempo tempo] [channel 0]))
(define harp   (new music-stream% [tempo tempo] [channel 4]))
(define bass   (new music-stream% [tempo tempo] [channel 3]))
(define strings (new music-stream% [tempo tempo] [channel 1]))
(define bells  (new music-stream% [tempo tempo] [channel 5]))
(define bells2 (new music-stream% [tempo tempo] [channel 5]))
(define drums  (new music-stream% [tempo tempo] [channel 9]))
(define cym    (new music-stream% [tempo tempo] [channel 9]))
(define cym2   (new music-stream% [tempo tempo] [channel 9]))

(define dev (new midi-output%))
(send dev open)

;; GM programs
(send dev program-change 0 0)    ; piano
(send dev program-change 48 1)   ; strings
(send dev program-change 89 2)   ; warm pad
(send dev program-change 43 3)   ; contrabass
(send dev program-change 46 4)   ; harp
(send dev program-change 14 5)   ; tubular bells

;; A little space: reverb send + chorus, and keep channels at full volume
(for ([ch (in-range 6)])
  (send dev control-change 7 100 ch)
  (send dev control-change 91 88 ch)
  (send dev control-change 93 36 ch))

(printf "=== NEST — Main Theme (D major, ~a BPM) ===~n" tempo)
(printf "Intro 2 bars | A 8 bars | B 8 bars | C 7 bars  (~a sec)~n"
        (round (* 104 (/ 60.0 tempo))))
(printf "Pad + piano + harp + contrabass + strings + bells + soft kit~n~n")

;; Pad starts at beat 0 and is the master clock.
(send pad start dev pad-notes)

;; Everything else enters at the downbeat after the 2-bar intro (beat 8).
(send piano start dev melody-notes #:from pad #:at-beat 8)
(send harp  start dev arp-notes    #:from pad #:at-beat 8)
(send bass  start dev bass-notes   #:from pad #:at-beat 8)
(send strings start dev string-notes #:from pad #:at-beat 40)
(send bells start dev bell-notes   #:from pad #:at-beat 72)
(send bells2 start dev bell-final-notes #:from pad #:at-beat 92)
(send drums start dev kick-notes   #:from pad #:at-beat 40)
(send cym   start dev cymbal-notes #:from pad #:at-beat 72)
(send cym2  start dev cymbal-final-notes #:from pad #:at-beat 92)

;; Wait for the final chord + tail, then stop.
(send pad wait-until 106)
(for ([s (list piano harp bass strings bells bells2 drums cym cym2 pad)])
  (send s stop))
(sleep 1.6)
(send dev close)
(printf "~nDone.~n"))
