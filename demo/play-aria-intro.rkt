#lang racket

;; 播放 Aria-AMT 前奏 RKT（4 小节，含 Pedal/CC64）
(require "../midi/main.rkt"
         "kaitou-aria-intro.rkt"
         "loop-m1m4.rkt")

;; 单次 M1–M4 也走循环函数 n=1：删除 M4 最后 3/16，但保留 Melody。
(loop-m1-m4 1 #:include-melody? #t)
(void)
