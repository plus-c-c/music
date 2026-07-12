#lang racket

;; ============================================================
;; 动态小节插入 & Stream 同步 专项测试
;; ============================================================

(require "midi/main.rkt")

(define passed 0)
(define failed 0)

(define (assert v [msg ""])
  (unless v (error 'test (string-append "assertion failed" msg))))

(define (test name thunk)
  (with-handlers ([exn:fail? (lambda (e)
                               (set! failed (+ failed 1))
                               (printf "FAIL ~a: ~a~n" name (exn-message e)))])
    (thunk)
    (set! passed (+ passed 1))
    (printf "PASS ~a~n" name)))

(define dev (new midi-output%))
(send dev open)
(printf "~n")

;; ==================== 素材 (均为4拍) ====================

(define (scale-up)
  (for/list ([p (list 60 62 64 65 67 69 71 72)])
    (midi-note p 100 0.5 0)))       ;; 8×0.5=4拍

(define (scale-down)
  (for/list ([p (list 72 71 69 67 65 64 62 60)])
    (midi-note p 90 0.5 0)))        ;; 8×0.5=4拍

(define (bass-line)
  (for/list ([p (list 36 36 41 41)])
    (midi-note p 80 1.0 0)))        ;; 4×1.0=4拍

(define (chord-prog)
  (list (midi-chord 60 'major 2.0 100 0)
        (midi-chord 65 'major 2.0 100 0)))  ;; 2×2.0=4拍

(define (drums)
  (list (kick 100 0.5)
        (hi-hat-closed 80 0.25)
        (snare 100 0.5)
        (hi-hat-closed 80 0.25)))   ;; 1.5拍

;; ==================== 测试 1: insert 对齐到下一小节边界 ====================

(test "insert: 对齐到下一小节边界"
  (lambda ()
    (define s (new music-stream% [tempo 120] [beats-per-measure 4]))
    ;; 4拍初始内容
    (define end0 (send s start dev (scale-up)))
    (assert (= end0 4.0) "初始结束拍应为4")

    ;; 插入 4 拍 → 应对齐到拍 4, 结束于拍 8
    (define end1 (send s insert (chord-prog) #:marker? #f))
    (printf "   end0=~a end1=~a~n" end0 end1)
    (assert (= end1 8.0) "插入后结束拍应为8")

    (send s stop)))

;; ==================== 测试 2: 连续多次插入 ====================

(test "insert: 连续3次插入"
  (lambda ()
    (define s (new music-stream% [tempo 120] [beats-per-measure 4]))
    (define end0 (send s start dev (scale-up)))
    (assert (= end0 4.0))

    ;; 第1次插入 → 对齐到4, 结束于8
    (define end1 (send s insert (chord-prog) #:marker? #f))
    (assert (= end1 8.0) "第1次插入")

    ;; 第2次插入 → 对齐到8, 结束于12
    (define end2 (send s insert (scale-down) #:marker? #f))
    (assert (= end2 12.0) "第2次插入")

    ;; 第3次插入 → 对齐到12, 结束于16
    (define end3 (send s insert (chord-prog) #:marker? #f))
    (assert (= end3 16.0) "第3次插入")

    (printf "   end: ~a → ~a → ~a → ~a~n" end0 end1 end2 end3)
    (send s stop)))

;; ==================== 测试 3: append-at ====================

(test "append-at: 紧接末尾追加"
  (lambda ()
    (define s (new music-stream% [tempo 120] [beats-per-measure 4]))
    (define end0 (send s start dev (scale-up)))
    (assert (= end0 4.0))

    ;; append 在拍 4.01 之后追加 4 拍 → 8.01 → measure-align → 12
    (define end1 (send s append-at (chord-prog)))
    (printf "   end0=~a end1=~a~n" end0 end1)
    (assert (= end1 12.0) "append后对齐到12")

    (send s stop)))

;; ==================== 测试 4: 播放中动态插入 ====================

(test "insert: 播放中动态插入"
  (lambda ()
    (define s (new music-stream% [tempo 120] [beats-per-measure 4]))
    ;; 8拍初始 (2×scale-up)
    (define end0 (send s start dev (append (scale-up) (scale-up))))
    (assert (= end0 8.0))

    ;; 等到拍 3 → 插入点应为拍 4 (下一小节)
    (send s wait-until 3.0)
    (define end1 (send s insert (chord-prog) #:marker? #f))
    (assert (= end1 12.0) "播放中插入")

    (send s wait-until end0)
    (sleep 0.3)
    (send s stop)))

;; ==================== 测试 5: 标志音 ====================

(test "insert: 标志音存在"
  (lambda ()
    (define s (new music-stream% [tempo 120] [beats-per-measure 4]))
    (send s start dev (scale-up))

    ;; 带 marker 插入 → 对齐到4, 结束于8
    (define end1 (send s insert (chord-prog) #:marker? #t))
    (assert (= end1 8.0) "marker插入对齐")

    ;; 不带 marker 插入 → 对齐到8, 结束于12
    (define end2 (send s insert (chord-prog) #:marker? #f))
    (assert (= end2 12.0) "no-marker插入对齐")

    (send s stop)))

;; ==================== 测试 6: 同步 - 基本 ====================

(test "sync: 两个stream同步播放"
  (lambda ()
    (define s1 (new music-stream% [tempo 120]))
    (define s2 (new music-stream% [tempo 120] [channel 1]))

    (define s1-end (send s1 start dev (scale-up)))
    (assert (= s1-end 4.0))

    ;; s2 从 s1 的拍 2 开始同步
    (send s2 start dev (scale-down)
          #:from s1
          #:at-beat 2.0)

    (sleep 0.5)
    (assert (send s1 is-running?))
    (assert (send s2 is-running?))

    (define b1 (send s1 current-beat))
    (define b2 (send s2 current-beat))
    (printf "   s1.beat=~a, s2.beat=~a~n"
            (~r b1 #:precision 1) (~r b2 #:precision 1))
    ;; s2 应稍慢于 s1 (从拍2开始)
    (assert (< b2 b1) "s2 beat应小于s1")

    (send s1 stop)
    (send s2 stop)))

;; ==================== 测试 7: 同步 - at-beat 小节对齐 ====================

(test "sync: at-beat 小节对齐"
  (lambda ()
    (define s1 (new music-stream% [tempo 120] [beats-per-measure 4]))
    (define s2 (new music-stream% [tempo 120] [channel 1]))

    (send s1 start dev (scale-up))  ;; 4拍

    ;; s2 从拍5开始 → 应对齐到拍8
    (send s2 start dev (scale-down)
          #:from s1
          #:at-beat 5.0)

    ;; 等到 s2 到达拍 8+ (120BPM: 8拍 = 4秒)
    (send s2 wait-until 9.0)
    (define b (send s2 current-beat))
    (printf "   s2.beat=~a (应≈9+)~n" (~r b #:precision 1))
    (assert (>= b 8.0) "s2应已过拍8")

    (send s1 stop)
    (send s2 stop)))

;; ==================== 测试 8: 同步 - 独立channel ====================

(test "sync: 独立channel播放"
  (lambda ()
    (define s1 (new music-stream% [tempo 120] [channel 0]))
    (define s2 (new music-stream% [tempo 120] [channel 3]))
    (define s3 (new music-stream% [tempo 120] [channel 5]))

    (send s1 start dev (scale-up))
    (send s2 start dev (bass-line)
          #:from s1 #:at-beat 0.0)
    (send s3 start dev (chord-prog)
          #:from s1 #:at-beat 4.0)

    (sleep 1.0)
    (assert (and (send s1 is-running?)
                 (send s2 is-running?)
                 (send s3 is-running?)))

    (send s1 stop)
    (send s2 stop)
    (send s3 stop)))

;; ==================== 测试 9: 插入与同步组合 ====================

(test "sync: 插入与同步组合"
  (lambda ()
    (define s1 (new music-stream% [tempo 120]))
    (define s2 (new music-stream% [tempo 120] [channel 1]))

    (define end0 (send s1 start dev (scale-up)))
    (assert (= end0 4.0))

    ;; s2 同步到 s1, 从拍 2 开始
    (send s2 start dev (bass-line)
          #:from s1 #:at-beat 2.0)

    ;; s1 在拍 3 等待后插入
    (send s1 wait-until 3.0)
    (define end1 (send s1 insert (chord-prog) #:marker? #f))
    (assert (= end1 8.0) "插入后结束拍")

    ;; s2 继续独立播放
    (sleep 1.0)
    (assert (send s2 is-running?) "s2仍运行")

    (send s1 wait-until end1)
    (sleep 0.3)
    (send s1 stop)
    (send s2 stop)))

;; ==================== 测试 10: 力度变化插入 ====================

(test "insert: 力度变化插入"
  (lambda ()
    (define s (new music-stream% [tempo 120] [beats-per-measure 4]))
    (send s start dev (scale-up))  ;; 4拍

    ;; 渐强插入 → 对齐到4, 结束于8
    (define end1 (send s insert
                       (crescendo (scale-up) 30 127)
                       #:marker? #f))
    (assert (= end1 8.0) "渐强插入对齐")

    ;; 渐弱插入 → 对齐到8, 结束于12
    (define end2 (send s insert
                       (decrescendo (scale-down) 127 30)
                       #:marker? #f))
    (assert (= end2 12.0) "渐弱插入对齐")

    (send s stop)))

;; ==================== 测试 11: 鼓组插入 ====================

(test "insert: 打击乐插入"
  (lambda ()
    (define s (new music-stream% [tempo 120] [beats-per-measure 4]))
    (send s start dev (scale-up))  ;; 4拍

    (define end1 (send s insert (drums) #:marker? #f))
    ;; drums = 1.5拍, 对齐到4, 结束于 ceil((4+1.5)/4)*4 = 8
    (assert (= end1 8.0) "鼓组插入对齐")

    (send s stop)))

;; ==================== 测试 12: tempo 变化 ====================

(test "set-tempo!: 变速播放"
  (lambda ()
    (define s (new music-stream% [tempo 120]))
    (define end0 (send s start dev (scale-up)))
    (assert (= end0 4.0))

    (send s set-tempo! 180)
    (assert (= (send s get-tempo) 180))

    (send s stop)))

;; ==================== 测试 13: 多流交替插入 ====================

(test "insert: 多流交替插入"
  (lambda ()
    (define s1 (new music-stream% [tempo 120] [channel 0]))
    (define s2 (new music-stream% [tempo 120] [channel 1]))

    (define e1 (send s1 start dev (scale-up)))  ;; 4拍
    (define e2 (send s2 start dev (bass-line)
                     #:from s1 #:at-beat 0.0))

    ;; 交替插入
    (send s1 wait-until 2.0)
    (define e1a (send s1 insert (chord-prog) #:marker? #f))
    (assert (= e1a 8.0))  ;; 对齐到4, +4拍=8

    (send s2 wait-until 2.0)
    (define e2a (send s2 insert (drums) #:marker? #f))
    (assert (= e2a 8.0))  ;; 对齐到4, +1.5拍→对齐到8

    (send s1 wait-until e1a)
    (sleep 0.3)
    (send s1 stop)
    (send s2 stop)))

;; ==================== 测试 14: beats-remaining ====================

(test "beats-remaining: 播放中递减"
  (lambda ()
    (define s (new music-stream% [tempo 120]))
    (send s start dev (scale-up))
    (sleep 0.5)
    (define r1 (send s beats-remaining))
    (sleep 1.0)
    (define r2 (send s beats-remaining))
    (printf "   r1=~a r2=~a~n"
            (~r r1 #:precision 1) (~r r2 #:precision 1))
    (assert (> r1 r2) "beats-remaining应递减")
    (send s stop)))

;; ==================== 清理 ====================

(send dev close)

(printf "~n=== 结果: ~a 通过, ~a 失败 ===~n" passed failed)
(when (> failed 0) (exit 1))
