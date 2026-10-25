#lang racket

;; ============================================================
;; build-aria-intro.rkt
;;
;; 从 Aria-AMT 的完整 27 秒 MIDI 中：
;;   - 自动检测音量明显增大的主旋律起点；
;;   - 只取前奏；
;;   - 自动按小节、声部拆分；Piano Low 不含谐波 G#2；Melody 为 D#6 主旋律；
;;   - 使用 1/16 全音符网格，RKT 时值分母最大为 16；
;;   - 把 Aria 的 CC64 踏板转成 RKT 的 pedal 标记；
;;   - 生成 kaitou-aria-intro.rkt / MIDI / WAV。
;; ============================================================

(require "../midi/main.rkt"
         "../midi-reader.rkt"
         "../midi-core.rkt"
         "../midi-voices.rkt"
         "../midi-file.rkt"
         "../midi-render.rkt"
         racket/string)

(define in-midi "demo/reference/aria-amt-full-75.mid")
(define out-rkt "demo/kaitou-aria-intro.rkt")
(define out-mid "demo/reference/aria-intro.mid")
(define out-wav "demo/reference/aria-intro.wav")
(define tempo 75)
(define ppq 480)
(define grid 1/4) ; 1/4 拍 = 1/16 全音符；保证分母 <= 16
(define bar-offset 5/4)
(define bar-length 4)

(define data (read-midi-data in-midi))
(define (sec->beat s) (* s (/ tempo 60.0)))
(define (q x) (quantize-beat x grid))
(define (value beats) (/ beats 4))

(struct anote (start end pitch velocity) #:transparent)

(define notes
  (for/list ([n (in-list (midi-data-notes data))])
    (define raw-s (midi-note-event-start-beat n))
    (define raw-e (midi-note-event-end-beat n))
    (define pitch (midi-note-event-pitch n))
    (define s (max 0 (q raw-s)))
    (define e (max (+ s grid) (q raw-e)))
    (anote s e pitch (midi-note-event-velocity n))))

;; ------------------------------------------------------------
;; 自动找主旋律起点：以“小节最大 velocity > 60”为音量增大
;; ------------------------------------------------------------
(define (bar-notes i)
  (define s (+ bar-offset (* i bar-length)))
  (filter (lambda (n) (and (>= (anote-start n) s)
                           (< (anote-start n) (+ s bar-length))))
          notes))

(define loud-bar
  (for/or ([i (in-range 7)])
    (define ns (bar-notes i))
    (and (pair? ns)
         (> (apply max (map anote-velocity ns)) 60)
         i)))

(define intro-bars (or loud-bar 7))
(define intro-end (+ bar-offset (* intro-bars bar-length)))
(printf "loud-bar index=~a, intro-bars=~a, intro end=~a beats\n"
        loud-bar intro-bars intro-end)

(define intro-notes
  (filter (lambda (n) (< (anote-start n) intro-end)) notes))
(define pickup-notes
  (filter (lambda (n) (< (anote-start n) bar-offset)) intro-notes))
(define main-notes
  (filter (lambda (n) (>= (anote-start n) bar-offset)) intro-notes))

;; ------------------------------------------------------------
;; 自动声部：每个 onset 最高音 / 最低音 / 内声部
;; ------------------------------------------------------------
(define groups (make-hash))
(for ([n (in-list main-notes)])
  (hash-update! groups (anote-start n) (lambda (xs) (cons n xs)) '()))

(define top-notes '())
(define low-notes '())
(define inner-notes '())
(for ([s (in-list (sort (hash-keys groups) <))])
  (define ns (sort (hash-ref groups s) > #:key anote-pitch))
  (define top (and (>= (anote-pitch (car ns)) 64) (car ns)))
  (when top
    (set! top-notes (cons top top-notes)))
  (define rest (if top (cdr ns) ns))
  (define low (and (pair? rest)
                   (let ([cand (car (reverse rest))])
                     (and (<= (anote-pitch cand) 60) cand))))
  (when low
    (set! low-notes (cons low low-notes))
    (set! rest (remove low rest)))
  (set! inner-notes (append rest inner-notes)))

(define (line->anotes line)
  (sort (for/list ([n (in-list (voice-line-notes line))])
          (anote (voice-note-start n) (voice-note-end n)
                 (voice-note-pitch n) (voice-note-velocity n)))
        < #:key anote-start))

(define inner-lines
  (map line->anotes
       (split-notes-into-voices
        (for/list ([n (in-list inner-notes)])
          (voice-note (anote-start n) (anote-end n)
                      (anote-pitch n) (anote-velocity n)))
        #:grid grid)))

(define (truncate-line ns)
  (define sorted (sort ns < #:key anote-start))
  (for/list ([n (in-list sorted)]
             [next (in-list (append (cdr sorted) (list #f)))])
    (define e (if next
                  (min (anote-end n) (max (anote-start n) (anote-start next)))
                  (anote-end n)))
    (anote (anote-start n) e (anote-pitch n) (anote-velocity n))))

;; M4 高音只保留与 M1–M3 相同的 12 音节奏型；
;; 去掉 M4 的额外 D#6，避免它破坏高音节奏型。
(define m4-start (+ bar-offset (* 3 bar-length)))
(set! top-notes
      (filter (lambda (n)
                (not (and (>= (anote-start n) m4-start)
                          (= (anote-pitch n) 87))))
              top-notes))

;; M4 的 G5 要像 M1–M3 的对应音一样持续 1/8；去掉 D#6 后
;; 它原来的 1/16 会留下多余 rest。
(set! top-notes
      (for/list ([n (in-list top-notes)])
        (if (and (>= (anote-start n) m4-start)
                 (= (anote-pitch n) 79)
                 (= (anote-start n) (+ m4-start 3)))
            (anote (anote-start n) (+ (anote-start n) 1/2)
                   (anote-pitch n) (anote-velocity n))
            n)))

(set! top-notes (truncate-line top-notes))
(set! low-notes (truncate-line low-notes))
(set! inner-lines (map truncate-line inner-lines))

;; ------------------------------------------------------------
;; 按小节生成 measure 块
;; ------------------------------------------------------------
(define (wrap-tie item from? to?)
  (cond
    [(and from? to?) (tie-continue item)]
    [from? (tie-end item)]
    [to? (tie-start item)]
    [else item]))

;; 把一条单声部线按小节切片。切片跨过小节线时使用 tie-start/tie-end，
;; 播放时合并为一个事件；显示时保留 ~ 连音标记。
(define (line-bar-items ns start channel [extras '()])
  (define end (+ start bar-length))
  (define raw
    (for/list ([n (in-list (sort ns < #:key anote-start))]
               #:when (and (< (anote-start n) end)
                           (> (anote-end n) start)))
      (define s (max start (anote-start n)))
      (define e (min end (anote-end n)))
      (and (> e s) (list s e n))))
  (define segments
    (sort (filter values raw) < #:key car))
  (define cursor start)
  (define items '())
  (for ([seg (in-list segments)])
    (define s (max cursor (car seg)))
    (define e (cadr seg))
    (define n (caddr seg))
    (when (> s cursor)
      (set! items (append items (list (rest (value (- s cursor)))))))
    (when (> e s)
      (define extra-pitches
        (for/list ([x (in-list extras)]
                   #:when (and (= (anote-start x) (anote-start n))
                               (not (= (anote-pitch x) (anote-pitch n)))))
          (anote-pitch x)))
      (define extra-vels
        (for/list ([x (in-list extras)]
                   #:when (= (anote-start x) (anote-start n)))
          (anote-velocity x)))
      (define chord-velocity
        (apply max (cons (anote-velocity n) extra-vels)))
      (define pitches
        (sort (remove-duplicates
               (cons (anote-pitch n) extra-pitches))
              <))
      (define base
        (if (pair? (cdr pitches))
            (chord-of (value (- e s)) pitches chord-velocity channel)
            (note (value (- e s)) (car pitches)
                  (anote-velocity n) channel)))
      (set! items
            (append items
                    (list (wrap-tie base
                                    (< (anote-start n) start)
                                    (and (> (anote-end n) end)
                                         (< end intro-end))))))
      (set! cursor e)))
  (when (> end cursor)
    (set! items (append items (list (rest (value (- end cursor)))))))
  items)

(define (line->measures ns channel [extras '()])
  (for/list ([i (in-range intro-bars)])
    (define s (+ bar-offset (* i bar-length)))
    (define items (line-bar-items ns s channel extras))
    (apply measure (add1 i) items)))

;; 弱起三音按音区直接并入 Piano High / Piano Low。
;; 去掉 Aria 中由踏板造成的过长时值：低音 D#3 到 M1 小节线，
;; 高音 A#4/D#5 依次到下一个弱起/M1 小节线，避免重叠。
(define low-pickup-items
  (list (rest (value 1/2))                 ; 0.5 拍前休止
        (tie-start (note (value 3/4) 51 45 1)))) ; D#3 3/16，tie 入 M1

(define high-pickup-items
  (list (rest (value 3/4))                 ; 到 0.75 拍
        (note (value 1/4) 70 35 0)          ; A#4 0.75
        (note (value 1/4) 75 45 0)))        ; D#5 1.00–1.25

(define (measures-part name line channel [extras '()] [pickup-items '()])
  ;; 有弱起的声部用 pickup-items 填到 1.25 拍；没有弱起的声部补
  ;; 小节起点前的休止，保证所有声部都从同一小节线开始。
  (define prelude
    (if (null? pickup-items)
        (list (rest (value bar-offset)))
        pickup-items))
  (apply part name
         (append prelude
                 (line->measures line channel extras))))

(define high-part (measures-part "Piano High" top-notes 0 '() high-pickup-items))
;; G#2 判定为 G#1 的谐波，不再作为独立低音或旋律音。
;; D#3 的 M1 rest 13/16 改为 D#3 的 tie continuation。
(define low-measures (line->measures low-notes 1))
(define low-m1
  (let* ([m (car low-measures)]
         [items (score-measure-items m)])
    (unless (and (pair? items) (midi-rest? (car items)))
      (error 'low-part "M1 第一项不是 rest"))
    (apply measure 1
           (cons (tie-end (note 13/16 51 45 1))
                 (cdr items)))))
(define low-part
  (apply part "Piano Low"
         (append low-pickup-items
                 (cons low-m1 (cdr low-measures)))))
;; Melody 只保留 D#6；G#2 不再重复写入。
(define melody-notes
  (list (anote (+ m4-start 13/4) (+ m4-start 4) 87 55)))
(define melody-parts
  (list (measures-part "Melody" melody-notes 2)))

;; ------------------------------------------------------------
;; 踏板：CC64 on/off -> pedal 持续时值，按小节拆分
;; ------------------------------------------------------------
;; M1–M3 统一为同一个小节踏板型：
;;   长段到 3.25，rest 1/8，短段 3/16 并 tie 到下一小节。
;; M4 保留收尾 rest。
(define pedal-intervals
  (list (cons 1 9/2)       ; 1.00–4.50
        (cons 5 17/2)      ; 5.00–8.50
        (cons 9 25/2)      ; 9.00–12.50
        (cons 13 33/2)))   ; 13.00–16.50

(define (pedal-bar-items intervals start)
  (define end (+ start bar-length))
  (define raw
    (for/list ([iv (in-list intervals)]
               #:when (and (< (car iv) end) (> (cdr iv) start)))
      (define s (max start (car iv)))
      (define e (min end (cdr iv)))
      (and (> e s) (list s e iv))))
  (define segments (sort (filter values raw) < #:key car))
  (define cursor start)
  (define items '())
  (for ([seg (in-list segments)])
    (define s (max cursor (car seg)))
    (define e (cadr seg))
    (define iv (caddr seg))
    (when (> s cursor)
      (set! items (append items (list (rest (value (- s cursor)))))))
    (when (> e s)
      (define base (pedal (value (- e s))))
      (set! items
            (append items
                    (list (wrap-tie base
                                    (< (car iv) start)
                                    (and (> (cdr iv) end)
                                         (< end intro-end))))))
      (set! cursor e)))
  (when (> end cursor)
    (set! items (append items (list (rest (value (- end cursor)))))))
  items)

(define pedal-measures
  (for/list ([i (in-range intro-bars)])
    (define s (+ bar-offset (* i bar-length)))
    (apply measure (add1 i)
           (pedal-bar-items pedal-intervals s))))

(define pedal-part
  (apply part "Pedal"
         (append (list (rest (value 1))          ; 0–1.0 拍
                       (tie-start (pedal (value 1/4)))) ; 1.0–1.25 拍
                 pedal-measures)))

(define all-parts (append (list high-part low-part) melody-parts (list pedal-part)))
(define voices all-parts)
(define piece (apply score all-parts))

;; ------------------------------------------------------------
;; 写 RKT
;; ------------------------------------------------------------
(define (write-base-item port item indent)
  (define pad (make-string indent #\space))
  (cond
    [(midi-rest? item)
     (fprintf port "~a(rest ~a)\n" pad (value (send item get-duration)))]
    [(midi-pedal? item)
     (fprintf port "~a(pedal ~a)\n" pad (value (send item get-duration)))]
    [(or (is-a? item midi-chord%) (midi-cluster? item))
     (define ns (chord-notes item))
     (define pitches (for/list ([n (in-list ns)]) (send n get-pitch)))
     (define first (car ns))
     (fprintf port "~a(chord-of ~a (list ~a) ~a ~a)\n"
              pad
              (value (send item get-duration))
              (string-join (map number->string pitches) " ")
              (send first get-velocity)
              (send first get-channel))]
    [else
     (define x (midi->note (send item get-pitch)))
     (fprintf port "~a(note ~a ~a ~a ~a) ; ~a~a\n"
              pad
              (value (send item get-duration))
              (send item get-pitch)
              (send item get-velocity)
              (send item get-channel)
              (car x) (cadr x))]))

(define (tie-form item)
  (cond
    [(and (tie-item-from? item) (tie-item-to? item)) "tie-continue"]
    [(tie-item-to? item) "tie-start"]
    [else "tie-end"]))

(define (write-tied port item indent)
  (define pad (make-string indent #\space))
  (fprintf port "~a(~a\n" pad (tie-form item))
  (write-base-item port (tie-base item) (+ indent 2))
  (fprintf port "~a)\n" pad))

(define (write-measure port form)
  (define n (score-measure-number form))
  (fprintf port "    (measure ~a\n" n)
  (for ([item (in-list (score-measure-items form))])
    (cond
      [(tie-item? item) (write-tied port item 6)]
      [else (write-base-item port item 6)]))
  (fprintf port "      )\n"))

(define (write-part port p idx)
  (fprintf port "(define voice-~a\n  (part \"~a\"\n" (add1 idx) (voice-name p))
  (for ([item (in-list (voice-items p))])
    (cond
      [(score-measure? item) (write-measure port item)]
      [(tie-item? item) (write-tied port item 4)]
      [else (write-base-item port item 4)]))
  (fprintf port "    ))\n\n"))

(call-with-output-file out-rkt
  (lambda (port)
    (fprintf port "#lang racket\n\n")
    (fprintf port ";; ============================================================\n")
    (fprintf port ";; kaitou-aria-intro.rkt — Aria-AMT 前奏（自动检测 ~a 小节）\n" intro-bars)
    (fprintf port ";; 小节：1..~a；bar-offset=~a 拍；不含音量增大的主旋律。\n" intro-bars bar-offset)
    (fprintf port ";; 声部：Piano High / Piano Low / Melody / Pedal；弱起三音已并入 High/Low。\n")
    (fprintf port ";; Piano Low 不含谐波 G#2；Melody 为 D#6 主旋律；弱起在 High/Low 内。\n")
    (fprintf port ";; 跨小节音符/踏板用 tie-start / tie-end / tie-continue 记录；\n")
    (fprintf port ";; pedal 与 note/rest 同用全音符分数；start 不写入数据。\n")
    (fprintf port ";; ============================================================\n\n")
    (fprintf port "(require \"../midi/main.rkt\")\n\n")
    (fprintf port "(provide tempo ppq bar-offset intro-bars voices piece)\n\n")
    (fprintf port "(define tempo ~a)\n" tempo)
    (fprintf port "(define ppq ~a)\n" ppq)
    (fprintf port "(define bar-offset ~a)\n" bar-offset)
    (fprintf port "(define intro-bars ~a)\n\n" intro-bars)
    (for ([p (in-list voices)] [i (in-naturals)])
      (write-part port p i))
    (fprintf port "(define voices\n  (list ~a))\n\n"
             (string-join (for/list ([i (in-range (length voices))])
                            (format "voice-~a" (add1 i))) " "))
    (fprintf port "(define piece (score ~a))\n"
             (string-join (for/list ([i (in-range (length voices))])
                            (format "voice-~a" (add1 i))) " ")))
  #:exists 'replace)

;; ------------------------------------------------------------
;; 写 MIDI（note + CC64）
;; ------------------------------------------------------------
(define rkt-events (score-events piece))
(define rkt-pedals (score-pedals piece))

(define mf (make-midi-file 0 ppq tempo))
(send mf add-track-name "Aria intro, 75 BPM")
(send mf add-tempo tempo)
(send mf add-program-change 0 0)
(define msgs
  (sort
   (append
    (append*
     (for/list ([p (in-list rkt-pedals)])
       (define s (inexact->exact (round (* (pedal-event-start p) ppq))))
       (define e (inexact->exact (round (* (+ (pedal-event-start p)
                                             (pedal-event-duration p)) ppq))))
       (list (list s 0 (+ #xB0 (pedal-event-channel p)) 64 127)
             (list e 1 (+ #xB0 (pedal-event-channel p)) 64 0))))
    (append*
     (for/list ([e (in-list rkt-events)])
       (define s (inexact->exact (round (* (midi-event-start e) ppq))))
       (define end (inexact->exact (round (* (+ (midi-event-start e)
                                                (midi-event-duration e)) ppq))))
       (define ch (midi-event-channel e))
       (list (list s 2 (+ #x90 ch) (midi-event-pitch e) (midi-event-velocity e))
             (list end 1 (+ #x80 ch) (midi-event-pitch e) 0)))))
   (lambda (a b)
     (or (< (car a) (car b))
         (and (= (car a) (car b)) (< (cadr a) (cadr b)))))))
(define last 0)
(for ([m (in-list msgs)])
  (define tick (car m))
  (send mf add-event (- tick last) (caddr m) (cadddr m) (list-ref m 4))
  (set! last tick))
(send mf save out-mid)
(render-midi-to-wav out-mid out-wav)
(printf "Wrote ~a, ~a, ~a; notes ~a, pedals ~a\n"
        out-rkt out-mid out-wav (length rkt-events) (length rkt-pedals))
