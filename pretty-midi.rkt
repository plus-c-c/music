#lang racket

;; ============================================================
;; pretty-midi.rkt — Racket 原生 MIDI 分析层
;;
;; 说明：pretty_midi 本身是 Python 库，不能直接加载进 Racket。
;; 这里在仓库内实现它当前需要的核心子集：
;;   - 读取 SMF，得到 note/start/end/pitch/velocity/channel
;;   - tempo / time-signature / track 名称
;;   - 按秒或按拍筛选音符
;;   - 按轨分组为 instrument
;;
;; 这样 MIDI 检查完全在 Racket 内完成，不依赖 Python。
;; ============================================================

(require "midi-reader.rkt"
         "midi-core.rkt")

(struct pm-instrument (track name notes) #:transparent)

(define sharp-pitch-names
  '#("C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"))

(define (pm-pitch-name pitch)
  (format "~a~a"
          (vector-ref sharp-pitch-names (modulo pitch 12))
          (- (quotient pitch 12) 1)))

;; ------------------------------------------------------------
;; 读取与全局信息
;; ------------------------------------------------------------
(define (read-pretty-midi path)
  (read-midi-data path))

(define (pm-notes pm)
  (midi-data-notes pm))

(define (pm-note-count pm)
  (length (pm-notes pm)))

(define (pm-end-time pm)
  (tick->second (midi-data-tempos pm)
                (midi-data-ppq pm)
                (midi-data-end-tick pm)))

(define (pm-tempo-changes pm)
  (for/list ([t (in-list (midi-data-tempos pm))])
    (list (midi-tempo-tick t)
          (/ 60000000.0 (midi-tempo-us-per-quarter t)))))

(define (pm-time-signatures pm)
  (for/list ([ts (in-list (midi-data-time-signatures pm))])
    (list (midi-time-signature-tick ts)
          (midi-time-signature-numerator ts)
          (midi-time-signature-denominator ts))))

(define (pm-track-name pm track)
  (define entry
    (for/first ([p (in-list (midi-data-track-names pm))]
                #:when (= (car p) track))
      (cdr p)))
  (or entry (format "Track ~a" track)))

(define (pm-instruments pm)
  (define notes (pm-notes pm))
  (define tracks
    (sort (remove-duplicates (map midi-note-event-track notes)) <))
  (for/list ([track (in-list tracks)])
    (pm-instrument track
                   (pm-track-name pm track)
                   (filter (lambda (n) (= (midi-note-event-track n) track))
                           notes))))

;; ------------------------------------------------------------
;; 音符访问器（秒 / 拍两种坐标）
;; ------------------------------------------------------------
(define (pm-note-start n) (midi-note-event-start-second n))
(define (pm-note-end n) (midi-note-event-end-second n))
(define (pm-note-start-beat n) (midi-note-event-start-beat n))
(define (pm-note-end-beat n) (midi-note-event-end-beat n))
(define (pm-note-pitch n) (midi-note-event-pitch n))
(define (pm-note-velocity n) (midi-note-event-velocity n))
(define (pm-note-channel n) (midi-note-event-channel n))
(define (pm-note-track n) (midi-note-event-track n))
(define (pm-note-name n) (pm-pitch-name (pm-note-pitch n)))

;; ------------------------------------------------------------
;; 范围筛选
;; ------------------------------------------------------------
(define (pm-notes-in-range pm start end)
  (filter (lambda (n)
            (and (>= (pm-note-start n) start)
                 (< (pm-note-start n) end)))
          (pm-notes pm)))

(define (pm-notes-in-beat-range pm start end)
  (filter (lambda (n)
            (and (>= (pm-note-start-beat n) start)
                 (< (pm-note-start-beat n) end)))
          (pm-notes pm)))

(define (pm-active-notes-in-beat-range pm start end)
  (filter (lambda (n)
            (and (< (pm-note-start-beat n) end)
                 (> (pm-note-end-beat n) start)))
          (pm-notes pm)))

;; ------------------------------------------------------------
;; 音高类统计
;; ------------------------------------------------------------
(define (pm-pitch-class-histogram notes)
  (define h (make-vector 12 0))
  (for ([n (in-list notes)])
    (define pc (modulo (pm-note-pitch n) 12))
    (vector-set! h pc (+ 1 (vector-ref h pc))))
  h)

(define (pm-pitch-class-cosine a b)
  (define dot (for/sum ([x (in-vector a)] [y (in-vector b)]) (* x y)))
  (define na (sqrt (for/sum ([x (in-vector a)]) (* x x))))
  (define nb (sqrt (for/sum ([y (in-vector b)]) (* y y))))
  (if (or (zero? na) (zero? nb))
      0.0
      (/ dot (* na nb))))

(define (pm-histogram->string h)
  (string-join
   (for/list ([pc (in-range 12)] [name (in-vector sharp-pitch-names)]
              #:when (> (vector-ref h pc) 0))
     (format "~a:~a" name (vector-ref h pc)))
   " "))

(provide pm-instrument pm-instrument?
         pm-instrument-track pm-instrument-name pm-instrument-notes
         read-pretty-midi
         pm-notes pm-note-count pm-end-time
         pm-tempo-changes pm-time-signatures
         pm-track-name pm-instruments
         pm-note-start pm-note-end
         pm-note-start-beat pm-note-end-beat
         pm-note-pitch pm-note-velocity pm-note-channel pm-note-track
         pm-note-name pm-pitch-name
         pm-notes-in-range pm-notes-in-beat-range
         pm-active-notes-in-beat-range
         pm-pitch-class-histogram pm-pitch-class-cosine
         pm-histogram->string)
