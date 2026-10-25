#lang racket

;; ============================================================
;; kaitou-aria-intro.rkt — Aria-AMT 前奏（自动检测 4 小节）
;; 小节：1..4；bar-offset=5/4 拍；不含音量增大的主旋律。
;; 声部：Piano High / Piano Low / Melody / Pedal；弱起三音已并入 High/Low。
;; Piano Low 不含谐波 G#2；Melody 为 D#6 主旋律；弱起在 High/Low 内。
;; 跨小节音符/踏板用 tie-start / tie-end / tie-continue 记录；
;; pedal 与 note/rest 同用全音符分数；start 不写入数据。
;; ============================================================

(require "../midi/main.rkt")

(provide tempo ppq bar-offset intro-bars voices piece)

(define tempo 75)
(define ppq 480)
(define bar-offset 5/4)
(define intro-bars 4)

(define voice-1
  (part "Piano High"
    (rest 3/16)
    (note 1/16 70 35 0) ; A#4
    (note 1/16 75 45 0) ; D#5
    (measure 1
      (note 1/8 82 55 0) ; A#5
      (note 1/16 70 35 0) ; A#4
      (note 1/16 75 45 0) ; D#5
      (note 1/8 82 45 0) ; A#5
      (note 1/16 70 35 0) ; A#4
      (note 1/16 75 45 0) ; D#5
      (note 1/8 77 45 0) ; F5
      (note 1/16 70 35 0) ; A#4
      (note 1/16 75 45 0) ; D#5
      (note 1/8 79 55 0) ; G5
      (note 1/16 70 35 0) ; A#4
      (note 1/16 74 50 0) ; D5
      )
    (measure 2
      (note 1/8 82 55 0) ; A#5
      (note 1/16 70 35 0) ; A#4
      (note 1/16 74 50 0) ; D5
      (note 1/8 82 45 0) ; A#5
      (note 1/16 70 35 0) ; A#4
      (note 1/16 74 50 0) ; D5
      (note 1/8 77 45 0) ; F5
      (note 1/16 70 35 0) ; A#4
      (note 1/16 74 50 0) ; D5
      (note 1/8 79 55 0) ; G5
      (note 1/16 72 45 0) ; C5
      (note 1/16 75 50 0) ; D#5
      )
    (measure 3
      (note 1/8 84 55 0) ; C6
      (note 1/16 72 45 0) ; C5
      (note 1/16 75 50 0) ; D#5
      (note 1/8 84 45 0) ; C6
      (note 1/16 72 45 0) ; C5
      (note 1/16 75 50 0) ; D#5
      (note 1/8 79 55 0) ; G5
      (note 1/16 72 45 0) ; C5
      (note 1/16 75 50 0) ; D#5
      (note 1/8 77 45 0) ; F5
      (note 1/16 70 40 0) ; A#4
      (note 1/16 74 50 0) ; D5
      )
    (measure 4
      (note 1/8 82 55 0) ; A#5
      (note 1/16 70 35 0) ; A#4
      (note 1/16 74 50 0) ; D5
      (note 1/8 82 45 0) ; A#5
      (note 1/16 70 35 0) ; A#4
      (note 1/16 74 50 0) ; D5
      (note 1/8 77 45 0) ; F5
      (note 1/16 70 35 0) ; A#4
      (note 1/16 74 50 0) ; D5
      (note 1/8 79 55 0) ; G5
      (note 1/16 72 45 0) ; C5
      (note 1/16 75 50 0) ; D#5
      )
    ))

(define voice-2
  (part "Piano Low"
    (rest 1/8)
    (tie-start
      (note 3/16 51 45 1) ; D#3
    )
    (measure 1
      (tie-end
        (note 13/16 51 45 1) ; D#3
      )
      (tie-start
        (note 3/16 46 45 1) ; A#2
      )
      )
    (measure 2
      (tie-end
        (note 13/16 46 45 1) ; A#2
      )
      (tie-start
        (note 3/16 48 45 1) ; C3
      )
      )
    (measure 3
      (tie-end
        (note 13/16 48 45 1) ; C3
      )
      (tie-start
        (note 3/16 43 45 1) ; G2
      )
      )
    (measure 4
      (tie-end
        (note 13/16 43 45 1) ; G2
      )
      (note 3/16 32 45 1) ; G#1
      )
    ))

(define voice-3
  (part "Melody"
    (rest 5/16)
    (measure 1
      (rest 1)
      )
    (measure 2
      (rest 1)
      )
    (measure 3
      (rest 1)
      )
    (measure 4
      (rest 13/16)
      (note 3/16 87 55 2) ; D#6
      )
    ))

(define voice-4
  (part "Pedal"
    (rest 1/4)
    (tie-start
      (pedal 1/16)
    )
    (measure 1
      (tie-end
        (pedal 13/16)
      )
      (rest 1/8)
      (tie-start
        (pedal 1/16)
      )
      )
    (measure 2
      (tie-end
        (pedal 13/16)
      )
      (rest 1/8)
      (tie-start
        (pedal 1/16)
      )
      )
    (measure 3
      (tie-end
        (pedal 13/16)
      )
      (rest 1/8)
      (tie-start
        (pedal 1/16)
      )
      )
    (measure 4
      (tie-end
        (pedal 13/16)
      )
      (rest 3/16)
      )
    ))

(define voices
  (list voice-1 voice-2 voice-3 voice-4))

(define piece (score voice-1 voice-2 voice-3 voice-4))
