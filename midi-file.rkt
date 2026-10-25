#lang racket

(require "midi-core.rkt")

;; MIDI 文件写入模块 (SMF Format 0/1)

(define midi-file%
  (class object%
    (super-new)
    (init-field
     [format 1]           ; MIDI 格式 (0=single, 1=multi-track)
     [ticks-per-beat 480] ; 每拍的 tick 数
     [tempo 120])         ; 默认 BPM

    (field
     [tracks '()]         ; 轨道列表
     [current-track '()]) ; 当前轨道事件

    ;; 添加事件到当前轨道
    ;; byte2 为 #f 时写成 2 字节消息（如 Program Change / 部分 Meta 事件）
    (define/public (add-event delta-time status byte1 [byte2 #f])
      (define data (if byte2
                       (list status byte1 byte2)
                       (list status byte1)))
      (set! current-track
            (cons (cons delta-time data) current-track)))

    ;; 添加音符事件
    (define/public (add-note pitch velocity duration-ticks [ch 0])
      (add-event 0 (+ NOTE-ON ch) pitch velocity)
      (add-event duration-ticks (+ NOTE-OFF ch) pitch 0))

    ;; 添加控制变更
    (define/public (add-control-change controller value [ch 0] [delta-time 0])
      (add-event delta-time (+ CONTROL-CHANGE ch) controller value))

    ;; 添加程序变更
    (define/public (add-program-change program [ch 0] [delta-time 0])
      (add-event delta-time (+ PROGRAM-CHANGE ch) program))

    ;; 添加弯音
    (define/public (add-pitch-bend value [ch 0] [delta-time 0])
      (let ([low (bitwise-and value #x7F)]
            [high (arithmetic-shift value -7)])
        (add-event delta-time (+ PITCH-BEND ch) low high)))

    ;; 添加元事件: 速度
    (define/public (add-tempo tempo-bpm [delta-time 0])
      (let ([microseconds (round (/ 60000000 tempo-bpm))])
        (add-event delta-time META-STATUS
                   (list 'meta 'tempo (list
                                       (bitwise-and (arithmetic-shift microseconds -16) #xFF)
                                       (bitwise-and (arithmetic-shift microseconds -8) #xFF)
                                       (bitwise-and microseconds #xFF))))))

    ;; 添加元事件: 轨道名称
    (define/public (add-track-name name [delta-time 0])
      (let ([name-bytes (string->bytes/latin-1 name)])
        (add-event delta-time META-STATUS
                   (list 'meta 'track-name name-bytes))))

    ;; 添加元事件: 乐器名称
    (define/public (add-instrument-name name [delta-time 0])
      (let ([name-bytes (string->bytes/latin-1 name)])
        (add-event delta-time META-STATUS
                   (list 'meta 'instrument-name name-bytes))))

    ;; 添加元事件: 时间签名
    (define/public (add-time-signature numerator denominator [delta-time 0])
      (let ([log-denom (inexact->exact (round (/ (log denominator) (log 2))))])
        (add-event delta-time META-STATUS
                   (list 'meta 'time-signature (list numerator log-denom 24 8)))))

    ;; 添加元事件: 调号
    (define/public (add-key-signature sf mi [delta-time 0])
      (add-event delta-time META-STATUS
                 (list 'meta 'key-signature (list sf mi))))

    ;; 添加元事件: 歌词
    (define/public (add-lyric text [delta-time 0])
      (let ([text-bytes (string->bytes/latin-1 text)])
        (add-event delta-time META-STATUS
                   (list 'meta 'lyric text-bytes))))

    ;; 添加元事件: 标记
    (define/public (add-marker text [delta-time 0])
      (let ([text-bytes (string->bytes/latin-1 text)])
        (add-event delta-time META-STATUS
                   (list 'meta 'marker text-bytes))))

    ;; 结束当前轨道并开始新轨道
    (define/public (new-track)
      (when (not (null? current-track))
        (set! tracks (cons (reverse current-track) tracks))
        (set! current-track '())))

    ;; 添加 MIDI 事件列表
    (define/public (add-events events [start-time 0])
      (let loop ([evts events]
                 [current-time start-time])
        (unless (null? evts)
          (let ([evt (car evts)])
            (cond
              [(and (list? evt) (= (length evt) 3))
               ;; MIDI 消息 (status byte1 byte2)
               (add-event 0 (car evt) (cadr evt) (caddr evt))
               (loop (cdr evts) current-time)]
              [(and (list? evt) (= (length evt) 2))
               ;; 时值
               (loop (cdr evts) (+ current-time (cadr evt)))]
              [else
               (loop (cdr evts) current-time)])))))

    ;; 保存到文件
    (define/public (save filename)
      ;; 结束最后一个轨道
      (when (not (null? current-track))
        (set! tracks (cons (reverse current-track) tracks))
        (set! current-track '()))

      (with-output-to-file filename
        (lambda ()
          ;; 写入文件头
          (write-midi-header)

          ;; 写入轨道
          (for-each (lambda (track) (write-track track)) tracks))
        #:exists 'replace))

    ;; 写入 MIDI 文件头
    (define/private (write-midi-header)
      ;; MThd 标识
      (write-bytes #"MThd")
      ;; 头部长度 (6 bytes)
      (write-u32be (current-output-port) 6)
      ;; 格式
      (write-u16be (current-output-port) format)
      ;; 轨道数
      (write-u16be (current-output-port) (length tracks))
      ;; 时间单位
      (write-u16be (current-output-port) ticks-per-beat))

    ;; 写入轨道
    (define/private (write-track track)
      ;; MTrk 标识
      (write-bytes #"MTrk")

      ;; 计算轨道数据
      (let ([track-data (open-output-bytes)])
        (parameterize ([current-output-port track-data])
          ;; 写入事件
          (for-each (lambda (event) (write-event event)) track)

          ;; 写入轨道结束标记
          (write-byte #x00)
          (write-byte META-STATUS)
          (write-byte #x2F)
          (write-byte #x00))

        ;; 写入轨道长度
        (let ([data (get-output-bytes track-data)])
          (write-u32be (current-output-port) (bytes-length data))
          (write-bytes data))))

    ;; 写入单个事件
    (define/private (write-event event)
      (let ([delta-time (car event)]
            [data (cdr event)])
        ;; 写入变长数量时间
        (write-bytes (encode-vlq delta-time))

        (cond
          ;; 元事件
          [(and (= (car data) META-STATUS)
                (list? (cadr data)))
           (write-byte META-STATUS)
           (let ([meta-type (cadr (cadr data))]
                 [meta-data (caddr (cadr data))])
             (write-byte (meta-type->byte meta-type))
             (cond
               [(bytes? meta-data)
                (write-bytes (encode-vlq (bytes-length meta-data)))
                (write-bytes meta-data)]
               [(list? meta-data)
                (write-bytes (encode-vlq (length meta-data)))
                (for-each write-byte meta-data)]
               [else
                (write-byte 0)]))]

          ;; 普通 MIDI 消息
          [(list? data)
           (for-each write-byte data)]

          [else
           (error 'write-event "无效的事件数据: ~a" data)])))

    ;; 元事件类型转换
    (define/private (meta-type->byte type)
      (case type
        [(sequence-number) #x00]
        [(text) #x01]
        [(copyright) #x02]
        [(track-name) #x03]
        [(instrument-name) #x04]
        [(lyric) #x05]
        [(marker) #x06]
        [(cue-point) #x07]
        [(channel-prefix) #x20]
        [(midi-port) #x21]
        [(end-of-track) #x2F]
        [(tempo) #x51]
        [(smpte-offset) #x54]
        [(time-signature) #x58]
        [(key-signature) #x59]
        [(sequencer-specific) #x7F]
        [else #x01]))))

;; 便捷构造函数
(define (make-midi-file [fmt 1] [tpb 480] [tempo-bpm 120])
  (new midi-file% [format fmt] [ticks-per-beat tpb] [tempo tempo-bpm]))

;; 从事件列表创建 MIDI 文件
(define (events->midi-file events filename [tempo-bpm 120] [ticks-per-beat 480])
  (let ([file (make-midi-file 0 ticks-per-beat tempo-bpm)])
    (send file add-track-name "Track 0")
    (send file add-tempo tempo-bpm)
    (for-each (lambda (evt)
                (cond
                  [(and (list? evt) (= (length evt) 3))
                   (send file add-note (cadr evt) (caddr evt) 0)]
                  [(and (list? evt) (= (length evt) 2)
                        (number? (cadr evt)))
                   ;; 时值，跳过
                   '()]))
              events)
    (send file save filename)))

;; 从音符列表创建 MIDI 文件
(define (notes->midi-file notes filename [tempo-bpm 120] [ticks-per-beat 480])
  (let ([file (make-midi-file 0 ticks-per-beat tempo-bpm)])
    (send file add-track-name "Track 0")
    (send file add-tempo tempo-bpm)
    (for-each (lambda (note)
                (let ([events (send note ->events ticks-per-beat)])
                  (for-each (lambda (evt)
                              (cond
                                [(and (list? evt) (= (length evt) 3))
                                 (send file add-note (cadr evt) (caddr evt) 0)]
                                [(and (list? evt) (= (length evt) 2)
                                      (number? (cadr evt)))
                                 ;; 时值，跳过
                                 '()]))
                            events)))
              notes)
    (send file save filename)))

;; 读取 MIDI 文件 (使用 midi-readwrite)
(define (read-midi-file filename)
  (dynamic-require 'midi-readwrite 'midi-file-parse)
  (let ([parser (dynamic-require 'midi-readwrite 'midi-file-parse)])
    (parser filename)))

(provide midi-file% make-midi-file
         events->midi-file notes->midi-file
         read-midi-file)
