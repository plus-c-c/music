#lang racket

;; ============================================================
;; midi-reader.rkt — SMF (Standard MIDI File) 读取与基础分析
;;
;; 只依赖仓库内的 midi-core.rkt；用于：
;;   - 读取 .mid / .midi
;;   - 把 note-on/note-off 配对成音符
;;   - 按拍 / 秒 / 小节统计
;;
;; 不处理 SMPTE 时间格式；当前项目的 MIDI 都是 PPQ。
;; ============================================================

(require "midi-core.rkt")

(struct midi-tempo (tick us-per-quarter) #:transparent)
(struct midi-time-signature (tick numerator denominator) #:transparent)

;; 控制器事件（当前主要保存 CC64 延音踏板）
(struct midi-pedal-event (tick second value channel track) #:transparent)

(struct midi-note-event
  (start-tick end-tick
   start-beat end-beat
   start-second end-second
   pitch velocity channel track)
  #:transparent)

(struct midi-data
  (format ppq tempos time-signatures notes track-names end-tick pedals)
  #:transparent)

;; ------------------------------------------------------------
;; 二进制读取
;; ------------------------------------------------------------
(define (read-chunk-id p)
  (define b (read-bytes 4 p))
  (if (eof-object? b) #f b))

(define (read-track-chunk p track-index)
  (define id (read-chunk-id p))
  (unless id (error 'read-midi-data "轨道头意外结束"))
  (define len (read-u32be p))
  (define chunk (read-bytes len p))
  (unless (bytes? chunk)
    (error 'read-midi-data "轨道 ~a 数据不完整" track-index))
  (if (equal? id #"MTrk")
      (parse-track (open-input-bytes chunk) track-index)
      (values '() '() '() '() 0 #f)))

;; ------------------------------------------------------------
;; 单轨解析
;; ------------------------------------------------------------
(define (parse-track p track-index)
  (define raw-events '())
  (define pedal-events '())
  (define tempos '())
  (define time-signatures '())
  (define track-name #f)
  (define end-tick 0)
  (define running-status #f)
  (define tick 0)
  (define done? #f)

  (let loop ()
    (unless done?
      (define delta (decode-vlq p))
      (set! tick (+ tick delta))
      (define first-byte (read-byte p))
      (cond
        [(eof-object? first-byte)
         (set! end-tick (max end-tick tick))
         (set! done? #t)]
        [else
         (cond
           ;; Meta event
           [(= first-byte #xFF)
            (set! running-status #f)
            (define meta-type (read-byte p))
            (define len (decode-vlq p))
            (define data (read-bytes len p))
            (cond
              [(= meta-type #x51) ; tempo
               (when (>= len 3)
                 (set! tempos
                       (cons (midi-tempo tick
                                         (+ (arithmetic-shift (bytes-ref data 0) 16)
                                            (arithmetic-shift (bytes-ref data 1) 8)
                                            (bytes-ref data 2)))
                             tempos)))]
              [(= meta-type #x58) ; time signature
               (when (>= len 2)
                 (set! time-signatures
                       (cons (midi-time-signature
                              tick
                              (bytes-ref data 0)
                              (expt 2 (bytes-ref data 1)))
                             time-signatures)))]
              [(= meta-type #x03) ; track name
               (set! track-name (bytes->string/latin-1 data))]
              [(= meta-type #x2F) ; end of track
               (set! end-tick (max end-tick tick))
               (set! done? #t)]
              [else (void)])]
           ;; SysEx / system common
           [(= first-byte #xF0)
            (set! running-status #f)
            (define len (decode-vlq p))
            (read-bytes len p)]
           [(= first-byte #xF7)
            (set! running-status #f)
            (define len (decode-vlq p))
            (read-bytes len p)]
           [(= first-byte #xF1) (read-byte p)]
           [(= first-byte #xF2) (read-byte p) (read-byte p)]
           [(= first-byte #xF3) (read-byte p)]
           [(= first-byte #xF6) (void)]
           [(>= first-byte #xF8) (void)] ; realtime，SMF 中通常不出现
           [else
            ;; Channel event（含 running status）
            (define status
              (if (>= first-byte #x80)
                  (begin (set! running-status first-byte) first-byte)
                  (begin
                    (unless running-status
                      (error 'read-midi-data "轨道 ~a 出现无 running status 的数据字节" track-index))
                    running-status)))
            (define high (bitwise-and status #xF0))
            (define channel (bitwise-and status #x0F))
            (define data1
              (if (>= first-byte #x80) (read-byte p) first-byte))
            (define data2
              (if (or (= high #xC0) (= high #xD0))
                  #f
                  (read-byte p)))
            (when (or (= high #x80) (= high #x90))
              (set! raw-events
                    (cons (list tick high channel data1 (or data2 0) track-index)
                          raw-events)))
            (when (and (= high #xB0) (= data1 64))
              (set! pedal-events
                    (cons (list tick (or data2 0) channel track-index)
                          pedal-events)))])])
      (loop)))

  (values (reverse raw-events)
          (reverse pedal-events)
          (reverse tempos)
          (reverse time-signatures)
          end-tick
          track-name))

;; ------------------------------------------------------------
;; tick -> 秒（支持 tempo map）
;; ------------------------------------------------------------
(define (tick->second tempos ppq tick)
  (define sorted (sort tempos < #:key midi-tempo-tick))
  (let loop ([ts sorted] [last-tick 0] [us 500000] [seconds 0.0])
    (cond
      [(null? ts)
       (+ seconds (/ (* (- tick last-tick) us) 1000000.0 ppq))]
      [(<= (midi-tempo-tick (car ts)) tick)
       (define t (car ts))
       (define dt (- (midi-tempo-tick t) last-tick))
       (loop (cdr ts)
             (midi-tempo-tick t)
             (midi-tempo-us-per-quarter t)
             (+ seconds (/ (* dt us) 1000000.0 ppq)))]
      [else
       (+ seconds (/ (* (- tick last-tick) us) 1000000.0 ppq))])))

;; ------------------------------------------------------------
;; 读取整个文件
;; ------------------------------------------------------------
(define (read-midi-data path)
  (define p (open-input-file path #:mode 'binary))
  (define header-id (read-chunk-id p))
  (unless (equal? header-id #"MThd")
    (error 'read-midi-data "不是标准 MIDI 文件: ~a" path))
  (define header-len (read-u32be p))
  (define format (read-u16be p))
  (define ntrks (read-u16be p))
  (define division (read-u16be p))
  (when (>= division #x8000)
    (error 'read-midi-data "暂不支持 SMPTE 时间格式"))
  (when (> header-len 6)
    (read-bytes (- header-len 6) p))

  (define all-raw '())
  (define all-pedals '())
  (define all-tempos '())
  (define all-timesigs '())
  (define track-names '())
  (define track-end 0)

  (for ([i (in-range ntrks)])
    (define-values (raw pedals tempos timesigs end name)
      (read-track-chunk p i))
    (set! all-raw (append all-raw raw))
    (set! all-pedals (append all-pedals pedals))
    (set! all-tempos (append all-tempos tempos))
    (set! all-timesigs (append all-timesigs timesigs))
    (set! track-end (max track-end end))
    (when name
      (set! track-names (cons (cons i name) track-names))))

  (close-input-port p)

  (define max-tick
    (max track-end
         (if (null? all-raw)
             0
             (apply max (map car all-raw)))))

  (define active (make-hash))
  (define out '())

  (define (close-note! key end-tick)
    (define st (hash-ref active key '()))
    (unless (null? st)
      (define start-tick (caar st))
      (define velocity (cadar st))
      (hash-set! active key (cdr st))
      (define-values (track channel pitch) (apply values key))
      (set! out
            (cons (midi-note-event
                   start-tick end-tick
                   (/ start-tick (exact->inexact division))
                   (/ end-tick (exact->inexact division))
                   (tick->second all-tempos division start-tick)
                   (tick->second all-tempos division end-tick)
                   pitch velocity channel track)
                  out))))

  (for ([ev (in-list (sort all-raw
                           (lambda (a b)
                             (or (< (car a) (car b))
                                 (and (= (car a) (car b))
                                      (< (list-ref a 5) (list-ref b 5)))
                                 (and (= (car a) (car b))
                                      (= (list-ref a 5) (list-ref b 5))
                                      (< (list-ref a 3) (list-ref b 3)))))))])
    (match-define (list tick high channel pitch velocity track) ev)
    (define key (list track channel pitch))
    (cond
      [(= high #x90)
       (if (> velocity 0)
           (hash-update! active key
                         (lambda (st) (cons (list tick velocity) st))
                         '())
           (close-note! key tick))]
      [(= high #x80) (close-note! key tick)]
      [else (void)]))

  ;; 未闭合的音符在文件末尾收尾
  (for ([(key st) (in-hash active)])
    (for ([_ (in-list st)])
      (close-note! key max-tick)))

  (define pedal-out
    (for/list ([ev (in-list (sort all-pedals < #:key car))])
      (match-define (list tick value channel track) ev)
      (midi-pedal-event tick
                        (tick->second all-tempos division tick)
                        value channel track)))

  (midi-data format division
             (sort all-tempos < #:key midi-tempo-tick)
             (sort all-timesigs < #:key midi-time-signature-tick)
             (sort out
                   (lambda (a b)
                     (or (< (midi-note-event-start-tick a)
                            (midi-note-event-start-tick b))
                         (and (= (midi-note-event-start-tick a)
                                 (midi-note-event-start-tick b))
                              (< (midi-note-event-pitch a)
                                 (midi-note-event-pitch b))))))
             (reverse track-names)
             max-tick
             pedal-out))

;; ------------------------------------------------------------
;; 基础查询
;; ------------------------------------------------------------
(define (midi-data-note-count d)
  (length (midi-data-notes d)))

(define (notes-in-beat-range notes start end)
  (filter (lambda (n)
            (and (>= (midi-note-event-start-beat n) start)
                 (< (midi-note-event-start-beat n) end)))
          notes))

(provide midi-tempo midi-tempo?
         midi-tempo-tick midi-tempo-us-per-quarter
         midi-time-signature midi-time-signature?
         midi-time-signature-tick midi-time-signature-numerator
         midi-time-signature-denominator
         midi-pedal-event midi-pedal-event?
         midi-pedal-event-tick midi-pedal-event-second
         midi-pedal-event-value midi-pedal-event-channel
         midi-pedal-event-track
         midi-note-event midi-note-event?
         midi-note-event-start-tick midi-note-event-end-tick
         midi-note-event-start-beat midi-note-event-end-beat
         midi-note-event-start-second midi-note-event-end-second
         midi-note-event-pitch midi-note-event-velocity
         midi-note-event-channel midi-note-event-track
         midi-data midi-data?
         midi-data-format midi-data-ppq midi-data-tempos
         midi-data-time-signatures midi-data-notes
         midi-data-track-names midi-data-end-tick
         midi-data-pedals
         read-midi-data
         midi-data-note-count
         notes-in-beat-range
         tick->second)
