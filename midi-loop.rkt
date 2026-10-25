#lang racket

;; ============================================================
;; midi-loop.rkt — 循环展开与循环播放
;;
;; 一个 loop 拆成三段：
;;   start 段：loop 起点之前的弱起，只播放一次；
;;   loop  段：[start, start+length) 的主体，逐遍重复；
;;   end   段：最后一遍末尾的 end-length 拍，只播放一次。
;;
;; start 段和 end 段各自有独立的变换函数：
;;   #:start-transform (loop-segment -> loop-segment) 或函数列表
;;   #:end-transform   (loop-segment -> loop-segment) 或函数列表
;;
;; 函数列表按顺序作用于同一个 loop-segment，因此同一段可以叠加多个修饰。
;; 默认：start 原样保留；end 清空为休止。
;; 可跨循环边界合并踏板，让上一遍末尾材料延音进入下一遍。
;;
;; 所有时间参数都是“拍”，不是全音符分数。
;; ============================================================

(require "midi-score.rkt"
         racket/list)

(provide loop-segment
         loop-segment?
         loop-segment-events
         loop-segment-pedals
         loop-segment-start
         loop-segment-end
         empty-loop-segment
         loop-segment-hold-pedal
         loop-events
         loop-score
         play-loop
         merge-pedal-events)

;; 一段循环材料：绝对时间的事件/踏板，以及它在原谱中的起点和终点。
(struct loop-segment (events pedals start end) #:transparent)

(define (empty-loop-segment seg)
  (loop-segment '() '()
                (loop-segment-start seg)
                (loop-segment-end seg)))

;; 清空段落中的音符，只留下覆盖整段的踏板。
(define (loop-segment-hold-pedal seg [channel 0] [value 127])
  (define s (loop-segment-start seg))
  (define e (loop-segment-end seg))
  (loop-segment '()
                (if (> e s)
                    (list (pedal-event s (- e s) value channel))
                    '())
                s e))

;; ------------------------------------------------------------
;; 基本校验
;; ------------------------------------------------------------
(define (check-loop-args repeats start loop-length end-length pickup-window)
  (unless (and (exact-integer? repeats) (> repeats 0))
    (error 'loop-events "循环次数必须是正整数，收到: ~a" repeats))
  (unless (real? start)
    (error 'loop-events "start 必须是实数，收到: ~a" start))
  (unless (and (real? loop-length) (positive? loop-length))
    (error 'loop-events "length 必须是正数，收到: ~a" loop-length))
  (unless (and (real? end-length)
               (>= end-length 0)
               (<= end-length loop-length))
    (error 'loop-events "end-length 必须在 0 和 length 之间，收到: ~a"
           end-length))
  (when pickup-window
    (unless (and (real? pickup-window) (>= pickup-window 0))
      (error 'loop-events "pickup-window 必须是非负数，收到: ~a"
             pickup-window))))

;; ------------------------------------------------------------
;; 事件移动与尾部裁剪
;; ------------------------------------------------------------
(define (shift-event e beats)
  (midi-event (+ (midi-event-start e) beats)
              (midi-event-duration e)
              (midi-event-pitch e)
              (midi-event-velocity e)
              (midi-event-channel e)
              (midi-event-kind e)))

(define (shift-pedal p beats)
  (pedal-event (+ (pedal-event-start p) beats)
               (pedal-event-duration p)
               (pedal-event-value p)
               (pedal-event-channel p)))

(define (trim-event-tail e cut)
  (define s (midi-event-start e))
  (define en (+ s (midi-event-duration e)))
  (cond
    [(>= s cut) #f]
    [(> en cut)
     (midi-event s (- cut s)
                 (midi-event-pitch e)
                 (midi-event-velocity e)
                 (midi-event-channel e)
                 (midi-event-kind e))]
    [else e]))

(define (trim-pedal-tail p cut)
  (define s (pedal-event-start p))
  (define en (+ s (pedal-event-duration p)))
  (cond
    [(>= s cut) #f]
    [(> en cut)
     (pedal-event s (- cut s)
                  (pedal-event-value p)
                  (pedal-event-channel p))]
    [else p]))

;; ------------------------------------------------------------
;; 踏板区间
;; ------------------------------------------------------------
(define (pedal-overlaps? p start end)
  (and (< (pedal-event-start p) end)
       (> (+ (pedal-event-start p) (pedal-event-duration p)) start)))

(define (merge-pedal-events ps)
  (define sorted (sort ps < #:key pedal-event-start))
  (define merged
    (for/fold ([acc '()]) ([p (in-list sorted)])
      (cond
        [(null? acc) (list p)]
        [else
         (define prev (car acc))
         (define prev-start (pedal-event-start prev))
         (define prev-end (+ prev-start (pedal-event-duration prev)))
         (define p-end (+ (pedal-event-start p) (pedal-event-duration p)))
         (if (and (= (pedal-event-value prev) (pedal-event-value p))
                  (= (pedal-event-channel prev) (pedal-event-channel p))
                  (<= (pedal-event-start p) prev-end))
             (cons (pedal-event prev-start
                                (- (max prev-end p-end) prev-start)
                                (pedal-event-value prev)
                                (pedal-event-channel prev))
                   (cdr acc))
             (cons p acc))])))
  (reverse merged))

;; 把每一遍的最后一 pedal 与下一遍的第一 pedal 合并为一个连续区间。
;; 这样循环边界不会出现同 tick 的 CC64 off/on。
(define (bridge-pedal-passes passes)
  (define v (list->vector passes))
  (for ([i (in-range (sub1 (vector-length v)))])
    (define prev (vector-ref v i))
    (define curr (vector-ref v (add1 i)))
    (when (and (pair? prev) (pair? curr))
      (define pl (last prev))
      (define cf (first curr))
      (when (and (= (pedal-event-value pl) (pedal-event-value cf))
                 (= (pedal-event-channel pl) (pedal-event-channel cf)))
        (define pl-start (pedal-event-start pl))
        (define pl-end (+ pl-start (pedal-event-duration pl)))
        (define cf-end (+ (pedal-event-start cf)
                          (pedal-event-duration cf)))
        (vector-set! v i
                     (append (drop-right prev 1)
                             (list (pedal-event pl-start
                                                (- (max pl-end cf-end)
                                                   pl-start)
                                                (pedal-event-value pl)
                                                (pedal-event-channel pl)))))
        (vector-set! v (add1 i) (rest curr)))))
  (append* (vector->list v)))

;; 段变换可以是一个函数，也可以是函数列表。
;; 列表会按顺序作用在同一个 loop-segment 上。
(define (normalize-segment-transforms name transform)
  (cond
    [(procedure? transform) (list transform)]
    [(and (list? transform)
          (andmap procedure? transform))
     transform]
    [else
     (error name "变换必须是函数或函数列表，收到: ~a" transform)]))

(define (apply-segment-transform name transform seg)
  (for/fold ([result seg]) ([f (in-list (normalize-segment-transforms name transform))])
    (define next (f result))
    (unless (loop-segment? next)
      (error name "变换函数必须返回 loop-segment，收到: ~a" next))
    next))

;; ------------------------------------------------------------
;; 循环展开
;; ------------------------------------------------------------
(define (loop-events events pedals repeats
                     #:start start
                     #:length loop-length
                     #:end-length [end-length 0]
                     #:pickup-window [pickup-window #f]
                     #:start-transform [start-transform (lambda (s) s)]
                     #:end-transform [end-transform empty-loop-segment]
                     #:bridge-pedals? [bridge-pedals? #f])
  (check-loop-args repeats start loop-length end-length pickup-window)
  (define end (+ start loop-length))
  (define cut (- end end-length))
  (define pickup-start (- start (or pickup-window 0)))
  (define last-index (sub1 repeats))
  (define last-offset (* last-index loop-length))

  ;; 起始段：只包含弱起窗口内、且不进入 loop 主体的材料。
  (define start-events
    (cond
      [(not pickup-window) '()]
      [(zero? pickup-window) '()]
      [else
       (filter (lambda (e)
                 (and (< (midi-event-start e) start)
                      (>= (midi-event-start e) pickup-start)))
               events)]))

  (define start-pedals
    (cond
      [(not pickup-window) '()]
      [(zero? pickup-window) '()]
      [else
       (filter (lambda (p)
                 (and (>= (pedal-event-start p) pickup-start)
                      (<= (+ (pedal-event-start p)
                             (pedal-event-duration p))
                          start)))
               pedals)]))

  ;; loop 段：完整主体。
  (define body-events
    (filter (lambda (e)
              (and (>= (midi-event-start e) start)
                   (< (midi-event-start e) end)))
            events))

  (define body-pedals
    (filter (lambda (p) (pedal-overlaps? p start end)) pedals))

  ;; end 段：最后一遍末尾的尾部材料。
  (define end-events
    (if (> end-length 0)
        (filter (lambda (e)
                  (and (>= (midi-event-start e) cut)
                       (< (midi-event-start e) end)))
                events)
        '()))

  (define end-pedals
    (if (> end-length 0)
        (filter (lambda (p)
                  (and (>= (pedal-event-start p) cut)
                       (< (pedal-event-start p) end)))
                pedals)
        '()))

  (define start-segment
    (apply-segment-transform
     'loop-events-start-transform
     start-transform
     (loop-segment start-events start-pedals pickup-start start)))

  (define end-segment
    (apply-segment-transform
     'loop-events-end-transform
     end-transform
     (loop-segment end-events end-pedals cut end)))

  ;; loop 段的最后一遍在 cut 处截止；end 段单独叠加。
  (define event-passes
    (for/list ([cycle (in-range repeats)])
      (define last? (= cycle last-index))
      (define trim? (and last? (> end-length 0)))
      (define offset (* cycle loop-length))
      (filter-map
       (lambda (e)
         (define e* (if trim? (trim-event-tail e cut) e))
         (and e* (shift-event e* offset)))
       body-events)))

  (define end-events-out
    (for/list ([e (in-list (loop-segment-events end-segment))])
      (shift-event e last-offset)))

  (define pedal-passes
    (for/list ([cycle (in-range repeats)])
      (define last? (= cycle last-index))
      (define trim? (and last? (> end-length 0)))
      (define offset (* cycle loop-length))
      (define body
        (filter-map
         (lambda (p)
           (define p* (if trim? (trim-pedal-tail p cut) p))
           (and p* (shift-pedal p* offset)))
         body-pedals))
      (define tail
        (if last?
            (for/list ([p (in-list (loop-segment-pedals end-segment))])
              (shift-pedal p last-offset))
            '()))
      (sort (append body tail) < #:key pedal-event-start)))

  (define looped-events
    (sort (append (loop-segment-events start-segment)
                  (append* event-passes)
                  end-events-out)
          < #:key midi-event-start))

  (define all-pedals
    (append (loop-segment-pedals start-segment)
            (if bridge-pedals?
                (bridge-pedal-passes pedal-passes)
                (append* pedal-passes))))

  (define looped-pedals
    (merge-pedal-events (sort all-pedals < #:key pedal-event-start)))

  (values looped-events looped-pedals))

;; ------------------------------------------------------------
;; 乐谱级 API
;; ------------------------------------------------------------
(define (select-voices score exclude-voices)
  (if (null? exclude-voices)
      score
      (filter (lambda (v) (not (member (voice-name v) exclude-voices)))
              score)))

(define (loop-score score repeats
                    #:start start
                    #:length loop-length
                    #:end-length [end-length 0]
                    #:pickup-window [pickup-window #f]
                    #:start-transform [start-transform (lambda (s) s)]
                    #:end-transform [end-transform empty-loop-segment]
                    #:exclude-voices [exclude-voices '()]
                    #:bridge-pedals? [bridge-pedals? #f]
                    #:legato [legato 1.0])
  (define score* (select-voices score exclude-voices))
  (loop-events (score-events score* legato)
               (score-pedals score*)
               repeats
               #:start start
               #:length loop-length
               #:end-length end-length
               #:pickup-window pickup-window
               #:start-transform start-transform
               #:end-transform end-transform
               #:bridge-pedals? bridge-pedals?))

(define (play-loop score repeats
                   #:start start
                   #:length loop-length
                   #:end-length [end-length 0]
                   #:pickup-window [pickup-window #f]
                   #:start-transform [start-transform (lambda (s) s)]
                   #:end-transform [end-transform empty-loop-segment]
                   #:exclude-voices [exclude-voices '()]
                   #:bridge-pedals? [bridge-pedals? #f]
                   #:legato [legato 1.0]
                   #:tempo [tempo (current-playback-tempo)]
                   #:backend [backend (current-playback-backend)])
  (define score* (select-voices score exclude-voices))
  (define-values (events pedals)
    (loop-events (score-events score* legato)
                 (score-pedals score*)
                 repeats
                 #:start start
                 #:length loop-length
                 #:end-length end-length
                 #:pickup-window pickup-window
                 #:start-transform start-transform
                 #:end-transform end-transform
                 #:bridge-pedals? bridge-pedals?))
  (play-events events (length score*)
               #:tempo tempo
               #:backend backend
               #:pedals pedals)
  (values events pedals))
