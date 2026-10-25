#lang racket

;; ============================================================
;; MIDI Music DSL — 统一入口
;; ============================================================
;;
;; 公共 API 分为以下几个模块:
;;
;;   Note     — 音符 (midi-note, midi-note-name)
;;   Chord    — 和弦 (midi-chord, midi-chord-name)
;;   Perc     — 打击乐 (kick, snare, hi-hat-closed, ...)
;;   Dynamics — 力度控制 (crescendo, decrescendo, ...)
;;   File     — MIDI 文件写入 (make-midi-file, save)
;;   Output   — 实时 MIDI 输出 (make-midi-output, open)
;;   Score    — 内部结构展开 (expand-music, expand-voices, midi-rest)
;;   Loop     — 循环展开与循环播放 (loop-events, loop-score, play-loop)
;;   Stream   — 节拍精确播放流 (music-stream%)
;;   Ports    — 端口枚举 (list-midi-out-ports)
;;

;; ---- 内部依赖 (不对外暴露) ----
(require "../midi-core.rkt"
         "../midi-note.rkt"
         "../midi-chord.rkt"
         "../midi-perc.rkt"
         "../midi-dynamics.rkt"
         "../midi-file.rkt"
         "../midi-output.rkt"
         "../midi-score.rkt"
         "../midi-loop.rkt"
         "../midi-dsl.rkt"
         "../music-stream.rkt"
         "../utils/rtmidi.rkt"
         "../mixer.rkt"
         "../effects.rkt"
         "../transport.rkt"
         "../metronome.rkt"
         "../session.rkt"
         "../automation.rkt"
         "../quantize.rkt"
         "../export.rkt")

;; ============================================================
;; Note — 音符
;; ============================================================
(provide
 ;; 类
 midi-note%

 ;; 构造函数
 ;;   (midi-note pitch [velocity 100] [duration 1.0] [channel 0])
 ;;   (midi-note-name "C" 4 100 1.0 0)
 midi-note
 midi-note-name

 ;; 序列工具
 midi-sequence)

;; ============================================================
;; Chord — 和弦
;; ============================================================
(provide
 ;; 类
 midi-chord%

 ;; 构造函数
 ;;   (midi-chord root [type 'major] [duration 1.0] [velocity 100] [channel 0])
 ;;   (midi-chord-name "C" 4 'major 1.0 100 0)
 midi-chord
 midi-chord-name)

;; ============================================================
;; Perc — 打击乐
;; ============================================================
(provide
 ;; 类
 midi-perc%

 ;; 构造函数
 ;;   (midi-perc drum [velocity 100] [duration 0.5] [channel 9])
 midi-perc

 ;; 快捷函数 (均返回 midi-perc% 对象)
 ;;   (kick [velocity 100] [duration 0.5])
 ;;   (snare ...)
 ;;   (hi-hat-closed ...)
 ;;   (hi-hat-open ...)
 ;;   (clap ...)
 ;;   (rim ...)
 ;;   (tom-high tom-mid tom-low)
 ;;   (crash ride cowbell tambourine)
 kick snare
 hi-hat-closed hi-hat-open
 clap rim
 tom-high tom-mid tom-low
 crash ride
 cowbell tambourine)

;; ============================================================
;; Dynamics — 力度控制
;; ============================================================
(provide
 ;; 乐句力度变换 (输入 note 列表, 返回新 note 列表)
 ;;   (crescendo notes start-vel [end-vel 127])
 ;;   (decrescendo notes start-vel [end-vel 1])
 ;;   (crescendo-decrescendo notes peak-vel [start-vel 1])
 crescendo
 decrescendo
 crescendo-decrescendo

 ;; 固定力度 / 增强 / 弱化
 ;;   (constant-velocity notes vel)
 ;;   (accent notes factor)
 ;;   (diminuendo notes factor)
 ;;   (sforzando notes accent-vel [normal-vel 60] [accent-pos 0])
 constant-velocity
 accent diminuendo
 sforzando

 ;; 力度标记映射: 'pp 'p 'mp 'm 'mf 'f 'ff → velocity
 ;;   (apply-dynamic notes 'mf)
 ;;   (hash-ref dynamics 'ff)
 apply-dynamic
 dynamics

 ;; 底层工具 (可用于自定义变换)
 ;;   (apply-velocity note vel) → new-note
 ;;   (scale-velocity-with-limit note factor) → new-note
 apply-velocity
 scale-velocity-with-limit)

;; ============================================================
;; File — MIDI 文件写入
;; ============================================================
(provide
 ;; 类
 midi-file%

 ;; 构造函数
 ;;   (make-midi-file [format 1] [ticks-per-beat 480] [tempo 120])
 make-midi-file

 ;; 快捷写入
 ;;   (notes->midi-file notes filename [tempo 120] [ticks-per-beat 480])
 ;;   (events->midi-file events filename [tempo 120] [ticks-per-beat 480])
 notes->midi-file
 events->midi-file)

;; ============================================================
;; Output — 实时 MIDI 输出
;; ============================================================
(provide
 ;; 类
 midi-output%

 ;; 构造函数
 ;;   (make-midi-output [channel 0] [port 0])
 make-midi-output

 ;; 列出并打印可用输出端口
 open-midi-ports)

;; ============================================================
;; Stream — 节拍精确播放流
;; ============================================================
(provide
 ;; 类 music-stream%
 ;;
 ;; 构造:
 ;;   (new music-stream% [tempo 120] [channel 0] [beats-per-measure 4])
 ;;
 ;; 方法:
 ;;   (send stream start dev notes
 ;;         [#:from source-stream]
 ;;         [#:at-beat beat])    — 启动播放, 返回结束拍
 ;;   (send stream stop)        — 停止
 ;;   (send stream insert notes [#:marker? #t])
 ;;                             — 在下一小节边界插入
 ;;   (send stream append-at notes)
 ;;                             — 紧接当前末尾追加
 ;;   (send stream wait-until beat)
 ;;                             — 阻塞等待到指定拍
 ;;   (send stream set-program dev program [ch])
 ;;   (send stream set-tempo! new-tempo)
 ;;   (send stream current-beat)   — 当前拍号
 ;;   (send stream beats-remaining) — 剩余拍数
 ;;   (send stream is-running?)
 ;;   (send stream get-tempo)
 ;;   (send stream beat->sec beats) — 拍→秒
 music-stream%)

;; ============================================================
;; Ports — MIDI 端口枚举
;; ============================================================
(provide
 ;; (list-midi-out-ports) → ((idx . name) ...)
 ;; (list-midi-in-ports)  → ((idx . name) ...)
 list-midi-out-ports
 list-midi-in-ports)

;; ============================================================
;; Constants — 仅暴露常用常量 (不暴露全部内部常量)
;; ============================================================
(provide
 ;; 通道
 channel-9)

;; ---- 辅助: 便捷的通道常量 ----
(define channel-9 9)

;; ============================================================
;; Mixer — 混音台
;; ============================================================
(provide
 ;; 类
 mixer% mixer-track% mixer-bus% mixer-vca%

 ;; 构造函数
 make-mixer make-track make-bus)

;; ============================================================
;; Effects — 效果器
;; ============================================================
(provide
 ;; 基类
 effect%

 ;; 内置效果器
 reverb% delay% eq% compressor% limiter%
 gain% pan-effect% gate% distortion% chorus%

 ;; 效果链
 make-effect-chain process-midi-effects)

;; ============================================================
;; Transport — 传输控制
;; ============================================================
(provide
 ;; 类
 transport%

 ;; 构造函数
 make-transport)

;; ============================================================
;; Metronome — 节拍器
;; ============================================================
(provide
 ;; 类
 metronome%

 ;; 构造函数
 make-metronome)

;; ============================================================
;; Session — 会话管理
;; ============================================================
(provide
 ;; 类
 session%

 ;; 构造函数
 make-session

 ;; 文件操作
 session-save session-load)

;; ============================================================
;; Automation — 参数自动化
;; ============================================================
(provide
 ;; 类
 automation% automation-point%

 ;; 构造函数
 make-automation

 ;; 操作
 add-point get-value-at

 ;; 插值函数
 linear-interpolate step-interpolate curve-interpolate)

;; ============================================================
;; Quantize — 量化
;; ============================================================
(provide
 ;; 核心函数
 quantize swing humanize velocity-quantize

 ;; 参数构造
 make-quantize-params

 ;; 预设网格
 quantize-grid swing-grid)

;; ============================================================
;; Export — 导出
;; ============================================================
(provide
 ;; 导出选项
 export-options%

 ;; 导出函数
 export-midi export-mixdown
 export-selected-tracks export-stems)

;; ============================================================
;; Score — 内部结构展开
;; ============================================================
(provide midi-event
         midi-event?
         midi-event-start
         midi-event-duration
         midi-event-pitch
         midi-event-velocity
         midi-event-channel
         midi-event-kind
         voice voice? voice-name voice-items
         expanded-voice expanded-voice?
         expanded-voice-name expanded-voice-items
         timed-item timed-item?
         timed-item-start timed-item-duration timed-item-object
         score-measure score-measure? score-measure-number score-measure-items measure
         pedal-event pedal-event? pedal-event-start pedal-event-duration
         pedal-event-value pedal-event-channel
         pedal midi-pedal?
         tie-start tie-end tie-continue
         tie-item? tie-base tie-item-from? tie-item-to?
         midi-rest midi-rest?
         midi-cluster midi-cluster?
         note-value
         beats->note-value
         whole half quarter eighth sixteenth thirty-second
         note chord chord-of rest perc tie
         part score n c r
         expand-voice
         expand-music
         expand-score
         expand-voices
         voice-events
         expanded-voice-events
         expanded-voice-pedals
         score-events
         chord-notes
         timed-item->pedal-events
         voice-pedals
         score-pedals
         loop-segment
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
         merge-pedal-events
         play-events
         play-score
         play-expanded-voice
         play-voice
         current-playback-backend
         current-playback-tempo
         current-soundfont)
