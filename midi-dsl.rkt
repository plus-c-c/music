#lang racket

;; ============================================================
;; midi-dsl.rkt — 以音符时值书写的 MIDI DSL
;;
;; 设计原则（Lisp 风格）：
;;   - 数据即代码：一个声部就是按顺序排列的 note / chord / rest / pedal / perc 列表
;;   - 声部内前后承接：不写 start，start 由 expand-voice 累加 duration 得到
;;   - 多声部 = score 里的多个 part，各自从 0 开始，展开时叠加
;;   - 时值统一用“全音符的分数”表示：
;;       1/1 = whole, 1/2 = half, 1/4 = quarter,
;;       1/8 = eighth, 1/16 = sixteenth
;;     附点/连音直接用分数表达：3/8 = dotted quarter, 1/12 = triplet eighth
;;
;; 术语：
;;   note value — 音符时值；rest — 休止；chord — 和弦；
;;   part/voice — 声部；score — 多声部乐谱；polyphony — 复调叠加。
;; ============================================================

(require "midi-note.rkt"
         "midi-chord.rkt"
         "midi-perc.rkt"
         "midi-score.rkt")

;; 常用音符时值常量（全音符 = 1）
(define whole 1)
(define half 1/2)
(define quarter 1/4)
(define eighth 1/8)
(define sixteenth 1/16)
(define thirty-second 1/32)

;; 全音符分数 -> 拍数（4/4 下 1/4 = 1 拍）
(define (note-value value [dots 0] [tuplet 1])
  (define base
    (cond
      [(number? value) (* 4 value)]
      [(eq? value 'whole) 4]
      [(eq? value 'half) 2]
      [(eq? value 'quarter) 1]
      [(eq? value 'eighth) 1/2]
      [(eq? value 'sixteenth) 1/4]
      [(eq? value 'thirty-second) 1/8]
      [else (error 'note-value "未知时值: ~a" value)]))
  (define dot-factor
    (for/sum ([i (in-range dots)]) (/ 1 (expt 2 (add1 i)))))
  (* base (+ 1 dot-factor) (/ 1 tuplet)))

;; 拍数 -> 时值（全音符的分数）；1/4 = 四分音符
(define (beats->note-value beats)
  (/ beats 4))

;; 单个音符
(define (note length pitch [velocity 100] [channel 0])
  (midi-note pitch velocity (note-value length) channel))

;; 和弦：root + type，或显式音符列表（见 chord-of）
(define (chord length root type [velocity 100] [channel 0])
  (midi-chord root type (note-value length) velocity channel))

;; 显式和弦：任意音高列表，避免受 chord type 限制
(define (chord-of length pitches [velocity 100] [channel 0])
  (midi-cluster (for/list ([p pitches])
                  (midi-note p velocity (note-value length) channel))
                (note-value length)))

;; 连音线：把同音高的若干音符合并成一个持续音（不重复触发）
(define (tie . notes)
  (unless (pair? notes)
    (error 'tie "tie 至少需要一个音符"))
  (define first (car notes))
  (unless (is-a? first midi-note%)
    (error 'tie "tie 只接受 midi-note: ~a" first))
  (define pitch (send first get-pitch))
  (define velocity (send first get-velocity))
  (define channel (send first get-channel))
  (for ([n notes])
    (unless (and (is-a? n midi-note%)
                 (= (send n get-pitch) pitch)
                 (= (send n get-velocity) velocity)
                 (= (send n get-channel) channel))
      (error 'tie "连音线内的音高/力度/通道必须一致")))
  (midi-note pitch velocity
             (for/sum ([n notes]) (send n get-duration))
             channel))

;; 休止
(define (rest length)
  (midi-rest (note-value length)))

;; 延音踏板：长度同样用全音符分数
(define (pedal length)
  (midi-pedal (note-value length)))

;; 打击乐
(define (perc length drum [velocity 100])
  (midi-perc drum velocity (note-value length)))

;; 声部：按顺序排列的事件
(define (part name . items)
  (voice name items))

;; 多声部乐谱
(define (score . parts)
  parts)

;; 短别名（可选，接近 Lisp 的紧凑写法）
(define n note)
(define c chord)
(define r rest)

(provide note-value
         beats->note-value
         whole half quarter eighth sixteenth thirty-second
         note chord chord-of rest pedal perc tie
         tie-start tie-end tie-continue
         part score
         n c r)
