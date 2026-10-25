#lang racket

;; MIDI Lang - 新的 DSL 接口
;; 保留旧接口用于向后兼容，同时提供新的 MIDI API

;; 新的 MIDI 模块
(require "midi-core.rkt")
(require "midi-note.rkt")
(require "midi-chord.rkt")
(require "midi-perc.rkt")
(require "midi-output.rkt")
(require "midi-file.rkt")
(require "midi-dynamics.rkt")
(require "midi-score.rkt")
(require "midi-dsl.rkt")

;; DAW 功能模块
(require "mixer.rkt")
(require "effects.rkt")
(require "transport.rkt")
(require "metronome.rkt")
(require "session.rkt")
(require "automation.rkt")
(require "quantize.rkt")
(require "export.rkt")

;; 旧模块 (保留用于向后兼容)
(require "volumn.rkt")

;; 导出新的 MIDI API
(provide midi-note midi-chord midi-perc)
(provide midi-note-name midi-chord-name)
(provide kick snare hi-hat-closed hi-hat-open clap rim)
(provide tom-high tom-mid tom-low crash ride cowbell tambourine)
(provide make-midi-output make-midi-file)
(provide midi-play midi-write midi-sequence)
(provide open-midi-ports)
(provide crescendo decrescendo crescendo-decrescendo)
(provide constant-velocity accent diminuendo)
(provide sforzando apply-dynamic dynamics)

;; 导出旧的音量常量
(provide (all-from-out "volumn.rkt"))

;; 导出 DAW 功能模块
(provide mixer% mixer-track% mixer-bus% mixer-vca%
         make-mixer make-track make-bus)
(provide effect% reverb% delay% eq% compressor% limiter%
         gain% pan-effect% gate% distortion% chorus%
         make-effect-chain process-midi-effects)
(provide transport% make-transport)
(provide metronome% make-metronome)
(provide session% make-session session-save session-load)
(provide automation% automation-point% make-automation
         add-point get-value-at)
(provide quantize swing humanize velocity-quantize
         make-quantize-params quantize-grid swing-grid)
(provide export-options% export-midi export-mixdown
         export-selected-tracks export-stems)
(provide midi-event midi-event? midi-event-start midi-event-duration
         midi-event-pitch midi-event-velocity midi-event-channel midi-event-kind
         voice voice? voice-name voice-items
         expanded-voice expanded-voice? expanded-voice-name expanded-voice-items
         timed-item timed-item? timed-item-start timed-item-duration timed-item-object
         midi-rest midi-rest?
         midi-cluster midi-cluster?
         note-value beats->note-value whole half quarter eighth sixteenth thirty-second
         note chord chord-of rest pedal perc tie
         tie-start tie-end tie-continue
         part score
         expand-voice expand-music expand-score expand-voices
         voice-events expanded-voice-events score-events chord-notes
         timed-item->pedal-events voice-pedals score-pedals
         play-events play-score play-expanded-voice play-voice
         current-playback-backend current-playback-tempo current-soundfont)

;; 全局 MIDI 输出
(define current-midi-output #f)

;; 获取或创建 MIDI 输出
(define (get-midi-output)
  (unless current-midi-output
    (set! current-midi-output (make-midi-output))
    (send current-midi-output open))
  current-midi-output)

;; 播放 MIDI 事件
(define (midi-play . events)
  (let ([output (get-midi-output)])
    (for-each (lambda (evt)
                (cond
                  ;; 音符对象
                  [(is-a? evt midi-note%)
                   (let ([events (send evt ->events 480)])
                     (send output play-events events))]
                  ;; 和弦对象
                  [(is-a? evt midi-chord%)
                   (let ([events (send evt ->events 480)])
                     (send output play-events events))]
                  ;; 打击乐对象
                  [(is-a? evt midi-perc%)
                   (let ([events (send evt ->events 480)])
                     (send output play-events events))]
                  ;; 列表
                  [(list? evt)
                   (for-each (lambda (e) (midi-play e)) evt)]
                  [else
                   (error 'midi-play "无效的事件类型: ~a" evt)]))
              events)))

;; 写入 MIDI 文件
(define (midi-write filename . events)
  (let* ([flat (flatten-events events)]
         [file (make-midi-file)])
    (send file add-track-name "Music")
    (send file add-tempo 120)
    (for-each (lambda (evt)
                (cond
                  ;; 音符对象
                  [(is-a? evt midi-note%)
                   (let ([pitch (send evt get-pitch)]
                         [velocity (send evt get-velocity)]
                         [duration (send evt get-duration)]
                         [channel (send evt get-channel)]
                         [ticks-per-beat 480])
                     (let ([duration-ticks (inexact->exact (round (* duration ticks-per-beat)))])
                       (send file add-note pitch velocity duration-ticks channel)))]
                  ;; 和弦对象
                  [(is-a? evt midi-chord%)
                   (let ([notes (send evt get-notes)])
                     (for-each (lambda (n)
                                 (let ([pitch (send n get-pitch)]
                                       [velocity (send n get-velocity)]
                                       [duration (send n get-duration)]
                                       [channel (send n get-channel)]
                                       [ticks-per-beat 480])
                                   (let ([duration-ticks (inexact->exact (round (* duration ticks-per-beat)))])
                                     (send file add-note pitch velocity duration-ticks channel))))
                               notes))]
                  ;; 打击乐对象
                  [(is-a? evt midi-perc%)
                   (let ([midi-num (send evt get-midi-number)]
                         [velocity (send evt get-velocity)]
                         [duration (send evt get-duration)]
                         [channel (send evt get-channel)]
                         [ticks-per-beat 480])
                     (when midi-num
                       (let ([duration-ticks (inexact->exact (round (* duration ticks-per-beat)))])
                         (send file add-note midi-num velocity duration-ticks channel))))]
                  [else
                   (error 'midi-write "无效的事件类型: ~a" evt)]))
              flat)
    (send file save filename)))

;; 扁平化事件列表
(define (flatten-events events)
  (cond
    [(null? events) '()]
    [(list? (car events))
     (append (flatten-events (car events))
             (flatten-events (cdr events)))]
    [else
     (cons (car events) (flatten-events (cdr events)))]))

;; 创建音符序列
(define (midi-sequence notes . args)
  (let ([ticks-per-beat (if (null? args) 480 (car args))])
    (apply append
           (map (lambda (n)
                  (cond
                    [(is-a? n midi-note%)
                     (send n ->events ticks-per-beat)]
                    [(is-a? n midi-chord%)
                     (send n ->events ticks-per-beat)]
                    [(is-a? n midi-perc%)
                     (send n ->events ticks-per-beat)]
                    [else '()]))
                notes))))

;; 从分数创建音符 (保持旧接口风格)
(define (note-from-fraction fraction pitch [velocity 100] [channel 0])
  (let ([duration (/ 1.0 fraction)])
    (midi-note pitch velocity duration channel)))

;; 从分数创建和弦
(define (chord-from-fraction fraction root type [velocity 100] [channel 0])
  (let ([duration (/ 1.0 fraction)])
    (midi-chord root type duration velocity channel)))

;; 从分数创建打击乐
(define (perc-from-fraction fraction drum [velocity 100] [channel 9])
  (let ([duration (/ 1.0 fraction)])
    (midi-perc drum velocity duration channel)))

;; 导出兼容旧接口的函数
(provide note-from-fraction chord-from-fraction perc-from-fraction)
