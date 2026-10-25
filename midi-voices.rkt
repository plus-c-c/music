#lang racket

;; ============================================================
;; midi-voices.rkt — 把一组带起止时间的音符拆成若干单声部线条
;;
;; 目标：
;;   - 输入是任意 polyphonic note 集合；
;;   - 输出若干 voice line，每一行内部严格前后承接；
;;   - 不产生 start 字段，后续由 DSL 的 part/note/rest 重新展开。
;;
;; 这不是“音乐分析”意义上的声部划分，而是保守的、可演奏的
;; monophonic voice allocation：优先复用已经结束的、音高最接近的声部。
;; ============================================================

(require "midi-core.rkt")

(struct voice-note (start end pitch velocity) #:transparent)
(struct voice-line (notes) #:transparent
  #:methods gen:custom-write
  [(define (write-proc v port mode)
     (fprintf port "#<voice-line: ~a notes>" (length (voice-line-notes v))))])

(struct working-voice (end last-pitch notes) #:mutable)

(define (quantize-beat x grid)
  ;; 源 MIDI 的拍点是浮点数；先转成精确整数个 grid，再乘回 grid
  (* grid (inexact->exact (round (/ (exact->inexact x)
                                    (exact->inexact grid))))))

(define (normalize-notes notes grid)
  (for/list ([n (in-list notes)])
    (define start (max 0 (quantize-beat (voice-note-start n) grid)))
    (define end (max (+ start grid)
                     (quantize-beat (voice-note-end n) grid)))
    (voice-note start end (voice-note-pitch n) (voice-note-velocity n))))

(define (split-notes-into-voices notes
                                 #:grid [grid 1/16])
  (define normalized
    (sort (normalize-notes notes grid)
          (lambda (a b)
            (or (< (voice-note-start a) (voice-note-start b))
                (and (= (voice-note-start a) (voice-note-start b))
                     (> (voice-note-pitch a) (voice-note-pitch b)))))))
  (define working '())
  (define (free? v start)
    (<= (working-voice-end v) start))
  (define (new-voice!)
    (define v (working-voice 0 #f '()))
    (set! working (cons v working))
    v)
  (for ([n (in-list normalized)])
    (define start (voice-note-start n))
    (define end (voice-note-end n))
    (define pitch (voice-note-pitch n))
    (define free (filter (lambda (v) (free? v start)) working))
    (define chosen
      (cond
        [(null? free) (new-voice!)]
        [else
         (argmin (lambda (v)
                   (define lp (working-voice-last-pitch v))
                   (if lp (abs (- lp pitch)) 1000))
                 free)]))
    (set-working-voice-end! chosen end)
    (set-working-voice-last-pitch! chosen pitch)
    (set-working-voice-notes! chosen (cons n (working-voice-notes chosen))))
  (for/list ([v (in-list (sort working
                               >
                               #:key
                               (lambda (v)
                                 (define ns (working-voice-notes v))
                                 (/ (for/sum ([n (in-list ns)])
                                      (voice-note-pitch n))
                                    (length ns)))) )])
    (voice-line (sort (working-voice-notes v)
                      <
                      #:key voice-note-start))))

(provide voice-note voice-note?
         voice-note-start voice-note-end
         voice-note-pitch voice-note-velocity
         voice-line voice-line?
         voice-line-notes
         quantize-beat
         normalize-notes
         split-notes-into-voices)
