#lang racket

;; 当前 Aria-AMT 前奏的 Lisp 环境。
;; require 后直接使用：
;;   piano-high / piano-low / melody / pedal-voice / aria-intro
;; 符号也注册进环境：
;;   'piano-high / 'piano-low / 'melody / 'pedal / 'aria-intro

(require "../midi-lisp.rkt"
         "kaitou-aria-intro.rkt")

;; 当前前奏的默认播放速度。
(current-playback-tempo 75)

(define (voice-by-name name)
  (or (for/first ([v (in-list voices)]
                  #:when (equal? (voice-name v) name))
        v)
      (error 'kaitou-lisp "找不到声部: ~a" name)))

(define-voice piano-high (voice-by-name "Piano High"))
(define-voice piano-low (voice-by-name "Piano Low"))
(define-voice melody (voice-by-name "Melody"))
(define-voice pedal-voice (voice-by-name "Pedal"))
(define-score aria-intro piece)

(void (bind-music! 'pedal pedal-voice))
(void (bind-music! 'piece piece))
(void (bind-music! 'voices voices))
(for ([name (in-list '(voice-1 voice-2 voice-3 voice-4))]
      [voice (in-list voices)])
  (void (bind-music! name voice)))

(provide (all-from-out "../midi-lisp.rkt")
         piano-high
         piano-low
         melody
         pedal-voice
         aria-intro)
