#lang racket

;; ============================================================
;; midi-score.rkt — 多声部乐谱结构 + 展开
;;
;; 结构：
;;   voice          — 一个声部 (name + 顺序排列的 DSL 对象)
;;   timed-item     — 展开后的“带拍点的对象” (起始拍、时值、原对象)
;;   expanded-voice — 展开后的声部 (name + timed-item 列表)
;;   midi-event     — 最终可播放/写文件的单音事件
;;
;; 一个声部里的对象可以是：
;;   midi-note% / midi-chord% / midi-perc% / midi-rest% / midi-pedal%
;;   score-measure / tie-item%（tie-start / tie-end / tie-continue）
;; chord 会展开为多个音符；rest 只推进时间；tie 片段展开时合并。
;; ============================================================

(require "midi-core.rkt"
         "midi-note.rkt"
         "midi-chord.rkt"
         "midi-perc.rkt"
         "midi-output.rkt"
         "midi-file.rkt"
         "utils/rtmidi.rkt"
         racket/system
         racket/file
         racket/path)

;; 一个可播放的单音事件
(struct midi-event (start duration pitch velocity channel kind) #:transparent)

;; 多声部乐谱：每个 voice 有自己的名字和顺序排列的 DSL 对象
(struct voice (name items) #:transparent)

;; 展开后的声部：items 是 timed-item 列表
(struct expanded-voice (name items) #:transparent)

;; 展开后的一个“乐谱项”：保留原始对象，便于检查音符/和弦/休止
(struct timed-item (start duration object) #:transparent)

;; 小节容器：只用于组织和检查，展开时按顺序拍平
(struct score-measure (number items) #:transparent)

(define (measure number . items)
  (score-measure number items))

(struct pedal-event (start duration value channel) #:transparent)

;; 休止符：只推进时间，不产生事件
(define midi-rest%
  (class object%
    (super-new)
    (init-field [duration 1.0])

    (define/public (get-duration) duration)
    (define/public (set-duration d)
      (new midi-rest% [duration (max 0 d)]))
    (define/public (->string)
      (format "#<midi-rest: ~a beats>" duration))))

(define (midi-rest duration)
  (new midi-rest% [duration duration]))

(define (midi-rest? x)
  (is-a? x midi-rest%))

;; 延音踏板标记：持续 duration 拍，展开为 CC64 on/off
(define midi-pedal%
  (class object%
    (super-new)
    (init-field [duration 1/4])

    (define/public (get-duration) duration)
    (define/public (set-duration d)
      (new midi-pedal% [duration (max 0 d)]))
    (define/public (->string)
      (format "#<midi-pedal: ~a beats>" duration))))

(define (midi-pedal duration)
  (new midi-pedal% [duration duration]))

(define (midi-pedal? x)
  (is-a? x midi-pedal%))

;; 跨小节延音：把一个普通对象拆成带 tie 标记的片段。
;;   tie-start    = 从本片段延续到下一片段
;;   tie-end      = 从前一片段延续到本片段
;;   tie-continue = 两端都延续
;; 片段仍按普通时值推进；展开/播放时合并成一个事件，不重复触发。
(define tie-item%
  (class object%
    (super-new)
    (init-field [item #f] [from? #f] [to? #f])

    (define/public (get-duration) (send item get-duration))
    (define/public (get-item) item)
    (define/public (tie-from?) from?)
    (define/public (tie-to?) to?)
    (define/public (->string)
      (format "#<tie-item: from=~a to=~a ~a>" from? to? item))))

(define (tie-start item)
  (new tie-item% [item item] [from? #f] [to? #t]))

(define (tie-end item)
  (new tie-item% [item item] [from? #t] [to? #f]))

(define (tie-continue item)
  (new tie-item% [item item] [from? #t] [to? #t]))

(define (tie-item? x)
  (is-a? x tie-item%))

;; 取掉 tie 包装，返回实际 note/pedal/rest 对象
(define (tie-base obj)
  (if (tie-item? obj) (send obj get-item) obj))

;; 显式和弦/音簇：任意 midi-note 列表同时发声
(define midi-cluster%
  (class object%
    (super-new)
    (init-field [notes '()] [duration 1.0])

    (define/public (get-notes) notes)
    (define/public (get-duration) duration)
    (define/public (set-duration d)
      (new midi-cluster% [notes notes] [duration (max 0 d)]))
    (define/public (->string)
      (format "#<midi-cluster: ~a notes, ~a beats>"
              (length notes) duration))))

(define (midi-cluster notes duration)
  (new midi-cluster% [notes notes] [duration duration]))

(define (midi-cluster? x)
  (is-a? x midi-cluster%))

(define (flatten-items xs)
  (cond
    [(null? xs) '()]
    [(score-measure? (car xs))
     (append (flatten-items (score-measure-items (car xs)))
             (flatten-items (cdr xs)))]
    [(list? (car xs)) (append (flatten-items (car xs))
                              (flatten-items (cdr xs)))]
    [else (cons (car xs) (flatten-items (cdr xs)))]))

;; 单个声部：返回 timed-item 列表（保留 note/chord/perc/rest 对象）
(define (expand-voice music [start 0.0])
  (let loop ([items (flatten-items music)] [t start] [acc '()])
    (cond
      [(null? items) (reverse acc)]
      [else
       (define obj (car items))
       (define dur (send obj get-duration))
       (loop (cdr items)
             (+ t dur)
             (cons (timed-item t dur obj) acc))])))

;; 一个 chord 的组成音符
(define (chord-notes chord)
  (send chord get-notes))

;; timed-item -> 单音事件列表
(define (timed-item->events ti [legato 1.0])
  (define obj (tie-base (timed-item-object ti)))
  (define s (timed-item-start ti))
  (define d (* (timed-item-duration ti) legato))
  (cond
    [(or (is-a? obj midi-rest%) (midi-pedal? obj)) '()]
    [(or (is-a? obj midi-chord%) (midi-cluster? obj))
     (for/list ([n (chord-notes obj)])
       (midi-event s d
                   (send n get-pitch)
                   (send n get-velocity)
                   (send n get-channel)
                   'note))]
    [(is-a? obj midi-perc%)
     (define p (send obj get-midi-number))
     (if p
         (list (midi-event s d p (send obj get-velocity) 9 'perc))
         '())]
    [else
     (list (midi-event s d
                       (send obj get-pitch)
                       (send obj get-velocity)
                       (send obj get-channel)
                       'note))]))

;; 取掉列表最后一个元素（不依赖 racket/list）
(define (remove-last xs)
  (reverse (cdr (reverse xs))))

(define (tie-item-to? obj)
  (and (tie-item? obj) (send obj tie-to?)))

(define (tie-item-from? obj)
  (and (tie-item? obj) (send obj tie-from?)))

(define (note-tie-open? obj)
  (and (tie-item-to? obj)
       (not (midi-pedal? (tie-base obj)))))

;; 合并跨小节 tie 的 timed-item 序列。
;; tie-start 产生一个事件；tie-end 不产生新事件，只延长前一个事件。
(define (timed-items->events tis [legato 1.0])
  (define result '())
  (define prev-ti #f)
  (define prev-events '())
  (for ([ti (in-list tis)])
    (define obj (timed-item-object ti))
    (define base (tie-base obj))
    (define from? (tie-item-from? obj))
    (when (and prev-ti
               (note-tie-open? (timed-item-object prev-ti))
               (not from?))
      (error 'voice-events "tie-start 后没有紧邻的 tie-end: ~a"
             (timed-item-object ti)))
    (cond
      [(midi-pedal? base) (void)]
      [from?
       (unless (and prev-ti (note-tie-open? (timed-item-object prev-ti)))
         (error 'voice-events "tie-end 前没有相邻的 tie-start: ~a" obj))
       (unless (= (length prev-events) 1)
         (error 'voice-events "跨小节 tie 只支持单音: ~a" obj))
       (define evs (timed-item->events ti legato))
       (unless (= (length evs) 1)
         (error 'voice-events "跨小节 tie 只支持单音: ~a" obj))
       (define pe (car prev-events))
       (define ce (car evs))
       (unless (and (= (midi-event-pitch pe) (midi-event-pitch ce))
                    (= (midi-event-velocity pe) (midi-event-velocity ce))
                    (= (midi-event-channel pe) (midi-event-channel ce)))
         (error 'voice-events "tie 两端的音高/力度/通道必须一致: ~a vs ~a" pe ce))
       (define new-end (+ (timed-item-start ti)
                          (* (timed-item-duration ti) legato)))
       (define merged
         (midi-event (midi-event-start pe)
                     (- new-end (midi-event-start pe))
                     (midi-event-pitch pe)
                     (midi-event-velocity pe)
                     (midi-event-channel pe)
                     (midi-event-kind pe)))
       (set! result (append (remove-last result) (list merged)))
       (set! prev-events (list merged))]
      [else
       (define evs (timed-item->events ti legato))
       (set! result (append result evs))
       (set! prev-events evs)])
    (set! prev-ti ti))
  (when (and prev-ti (note-tie-open? (timed-item-object prev-ti)))
    (error 'voice-events "tie-start 后没有 tie-end"))
  result)

;; 单个声部 -> 单音事件列表
(define (voice-events v [legato 1.0])
  (timed-items->events (expand-voice (voice-items v)) legato))

;; timed-item -> 踏板事件（只有 pedal 对象产生 CC64）
(define (timed-item->pedal-events ti)
  (define obj (tie-base (timed-item-object ti)))
  (if (midi-pedal? obj)
      (list (pedal-event (timed-item-start ti)
                         (timed-item-duration ti)
                         127 0))
      '()))

;; 踏板 tie 的合并规则与音符相同，但合并的是 CC64 区间。
(define (timed-items->pedals tis)
  (define result '())
  (define prev-ti #f)
  (define prev-events '())
  (for ([ti (in-list tis)])
    (define obj (timed-item-object ti))
    (define base (tie-base obj))
    (define from? (tie-item-from? obj))
    (when (and prev-ti
               (let ([po (timed-item-object prev-ti)])
                 (and (tie-item-to? po)
                      (midi-pedal? (tie-base po))))
               (not from?))
      (error 'voice-pedals "pedal tie-start 后没有紧邻的 pedal tie-end: ~a"
             (timed-item-object ti)))
    (cond
      [(not (midi-pedal? base)) (void)]
      [from?
       (unless (and prev-ti
                    (let ([po (timed-item-object prev-ti)])
                      (and (tie-item-to? po)
                           (midi-pedal? (tie-base po)))))
         (error 'voice-pedals "pedal tie-end 前没有相邻的 pedal tie-start: ~a" obj))
       (unless (= (length prev-events) 1)
         (error 'voice-pedals "pedal tie 状态错误: ~a" obj))
       (define evs (timed-item->pedal-events ti))
       (unless (= (length evs) 1)
         (error 'voice-pedals "pedal tie 只能包一个踏板: ~a" obj))
       (define pe (car prev-events))
       (define ce (car evs))
       (unless (= (pedal-event-channel pe) (pedal-event-channel ce))
         (error 'voice-pedals "pedal tie 两端通道必须一致"))
       (define new-end (+ (timed-item-start ti) (timed-item-duration ti)))
       (define merged
         (pedal-event (pedal-event-start pe)
                      (- new-end (pedal-event-start pe))
                      (pedal-event-value pe)
                      (pedal-event-channel pe)))
       (set! result (append (remove-last result) (list merged)))
       (set! prev-events (list merged))]
      [else
       (define evs (timed-item->pedal-events ti))
       (set! result (append result evs))
       (set! prev-events evs)])
    (set! prev-ti ti))
  (when (and prev-ti
             (let ([po (timed-item-object prev-ti)])
               (and (tie-item-to? po)
                    (midi-pedal? (tie-base po)))))
    (error 'voice-pedals "pedal tie-start 后没有 pedal tie-end"))
  result)

(define (voice-pedals v)
  (timed-items->pedals (expand-voice (voice-items v))))

(define (expanded-voice-pedals ev)
  (timed-items->pedals (expanded-voice-items ev)))

(define (score-pedals score)
  (append* (for/list ([v (in-list score)])
             (voice-pedals v))))

;; 顺序展开一段音乐 -> 单音事件列表
(define (expand-music music [start 0.0] [legato 1.0])
  (timed-items->events (expand-voice music start) legato))

;; 多声部乐谱 -> 展开后的声部列表（每个声部保留 timed-item 结构）
(define (expand-score score [start 0.0])
  (for/list ([v score])
    (expanded-voice (voice-name v)
                    (expand-voice (voice-items v) start))))

;; 多声部乐谱 -> 合并的单音事件列表
(define (score-events score [legato 1.0])
  (append* (for/list ([v score])
             (voice-events v legato))))

;; 兼容旧接口：把多个声部顺序展开后合并
(define (expand-voices voices [start 0.0] [legato 1.0])
  (sort
   (append* (for/list ([v voices])
              (expand-music v start legato)))
   < #:key midi-event-start))

;; 展开后的声部 -> 单音事件列表
(define (expanded-voice-events ev [legato 1.0])
  (timed-items->events (expanded-voice-items ev) legato))

;; ------------------------------------------------------------
;; 播放后端
;;
;; auto: 有真实 MIDI 合成器端口时用 MIDI；否则用 FluidSynth 渲染 WAV
;;       并通过系统音频播放器播放（避免只连到 Midi Through 没声音）。
;; ------------------------------------------------------------
(define current-playback-backend (make-parameter 'auto))
(define current-playback-tempo (make-parameter 76))
(define current-soundfont
  (make-parameter "/usr/share/soundfonts/FluidR3_GM.sf2"))

(define (midi-through-port? name)
  (and (regexp-match #rx"[Mm]idi [Tt]hrough" name) #t))

(define (audible-midi-port?)
  (for/or ([p (in-list (list-midi-out-ports))])
    (not (midi-through-port? (cdr p)))))

(define (resolve-backend backend)
  (cond
    [(eq? backend 'auto) (if (audible-midi-port?) 'midi 'audio)]
    [else backend]))

;; ------------------------------------------------------------
;; 实时 MIDI 后端
;; ------------------------------------------------------------
(define (play-events-midi evs tempo voice-count pedals)
  (define dev (new midi-output%))
  (send dev open)
  ;; 每个声部一个 channel；打击乐固定 9
  (for ([ch (remove-duplicates (map midi-event-channel evs))])
    (unless (= ch 9)
      (send dev program-change 0 ch)))
  (define msgs
    (sort
     (append
      (append*
       (for/list ([p (in-list pedals)])
         (list (list (pedal-event-start p) 0
                     (list (+ #xB0 (pedal-event-channel p)) 64 127))
               (list (+ (pedal-event-start p) (pedal-event-duration p)) 1
                     (list (+ #xB0 (pedal-event-channel p)) 64 0)))))
      (append*
       (for/list ([e evs])
         (define ch (midi-event-channel e))
         (list (list (midi-event-start e) 1
                     (list (+ #x90 ch) (midi-event-pitch e) (midi-event-velocity e)))
               (list (+ (midi-event-start e) (midi-event-duration e)) 0
                     (list (+ #x80 ch) (midi-event-pitch e) 0))))))
     (lambda (a b)
       (or (< (car a) (car b))
           (and (= (car a) (car b))
                (< (cadr a) (cadr b)))))))
  (define seconds-per-beat (/ 60.0 tempo))
  (define t0 (current-inexact-milliseconds))
  (for ([m msgs])
    (define target (+ t0 (* 1000.0 (car m) seconds-per-beat)))
    (define wait-ms (- target (current-inexact-milliseconds)))
    (when (> wait-ms 0) (sleep (/ wait-ms 1000.0)))
    (send dev send-message (caddr m)))
  (sleep 0.8)
  (send dev close)
  (printf "Played ~a voice~a, ~a note events. (MIDI)\n"
          voice-count (if (= voice-count 1) "" "s") (length evs)))

;; ------------------------------------------------------------
;; 离线 FluidSynth -> WAV -> 系统播放器
;; ------------------------------------------------------------
(define (beat->tick b [ppq 480])
  (inexact->exact (round (* b ppq))))

(define (write-playback-midi evs path tempo pedals)
  (define ppq 480)
  (define mf (make-midi-file 0 ppq tempo))
  (send mf add-track-name "Playback")
  (send mf add-tempo tempo)
  (for ([ch (remove-duplicates (map midi-event-channel evs))])
    (unless (= ch 9)
      (send mf add-program-change 0 ch)))
  (define msgs
    (sort
     (append
      (append*
       (for/list ([p (in-list pedals)])
         (define s (beat->tick (pedal-event-start p) ppq))
         (define e* (beat->tick (+ (pedal-event-start p)
                                   (pedal-event-duration p))
                                ppq))
         (list (list s 0 (+ #xB0 (pedal-event-channel p)) 64 127)
               (list e* 1 (+ #xB0 (pedal-event-channel p)) 64 0))))
      (append*
       (for/list ([e evs])
         (define ch (midi-event-channel e))
         (define p (midi-event-pitch e))
         (define s (beat->tick (midi-event-start e) ppq))
         (define e* (beat->tick (+ (midi-event-start e)
                                   (midi-event-duration e))
                                ppq))
         (list (list s 2 (+ #x90 ch) p (midi-event-velocity e))
               (list e* 1 (+ #x80 ch) p 0)))))
     (lambda (a b)
       (or (< (car a) (car b))
           (and (= (car a) (car b))
                (< (cadr a) (cadr b)))))))
  (define last 0)
  (for ([m (in-list msgs)])
    (define tick (car m))
    (send mf add-event (- tick last) (caddr m) (cadddr m) (list-ref m 4))
    (set! last tick))
  (send mf save path))

(define (render-midi-to-wav midi-path wav-path)
  (define fluidsynth (find-executable-path "fluidsynth"))
  (unless fluidsynth
    (error 'play-events "找不到 fluidsynth，请先安装 FluidSynth"))
  (define soundfont (current-soundfont))
  (unless (file-exists? soundfont)
    (error 'play-events "找不到 SoundFont: ~a" soundfont))
  (unless (system* (path->string fluidsynth)
                   "-q" "-ni" "-a" "file"
                   "-F" (path->string wav-path)
                   "-r" "44100"
                   soundfont
                   (path->string midi-path))
    (error 'play-events "FluidSynth 渲染失败")))

(define (play-wav wav-path)
  (define path (path->string wav-path))
  (define players
    (filter values
            (list
             (let ([p (find-executable-path "paplay")])
               (and p (list (path->string p) path)))
             (let ([p (find-executable-path "ffplay")])
               (and p (list (path->string p) "-nodisp" "-autoexit"
                            "-loglevel" "quiet" path)))
             (let ([p (find-executable-path "aplay")])
               (and p (list (path->string p) "-q" path))))))
  (unless (pair? players)
    (error 'play-events "找不到音频播放器（paplay / ffplay / aplay）"))
  (or (for/or ([cmd (in-list players)])
        (apply system* cmd))
      (error 'play-events "音频播放失败")))

(define (play-events-audio evs tempo voice-count pedals)
  (define mid (make-temporary-file "kaitou-play-~a.mid"))
  (define wav (string->path (string-append (path->string mid) ".wav")))
  (dynamic-wind
    void
    (lambda ()
      (write-playback-midi evs mid tempo pedals)
      (render-midi-to-wav mid wav)
      (play-wav wav))
    (lambda ()
      (when (file-exists? mid) (delete-file mid))
      (when (file-exists? wav) (delete-file wav))))
  (printf "Played ~a voice~a, ~a note events. (audio)\n"
          voice-count (if (= voice-count 1) "" "s") (length evs)))

;; ------------------------------------------------------------
;; 统一入口
;; ------------------------------------------------------------
(define (play-events evs [voice-count 1]
                    #:tempo [tempo (current-playback-tempo)]
                    #:backend [backend (current-playback-backend)]
                    #:pedals [pedals '()])
  (define actual (resolve-backend backend))
  (cond
    [(eq? actual 'midi) (play-events-midi evs tempo voice-count pedals)]
    [(eq? actual 'audio) (play-events-audio evs tempo voice-count pedals)]
    [else (error 'play-events "未知播放后端: ~a" actual)]))

;; 用库的播放后端播放多声部乐谱（展开前）
(define (play-score score [legato 1.0]
                    #:tempo [tempo (current-playback-tempo)]
                    #:backend [backend (current-playback-backend)])
  (play-events (score-events score legato) (length score)
               #:tempo tempo #:backend backend
               #:pedals (score-pedals score)))

;; 播放一个已展开的声部（展开后）
(define (play-expanded-voice ev [legato 1.0]
                             #:tempo [tempo (current-playback-tempo)]
                             #:backend [backend (current-playback-backend)])
  (play-events (expanded-voice-events ev legato) 1
               #:tempo tempo #:backend backend))

(define (play-voice v [legato 1.0]
                    #:tempo [tempo (current-playback-tempo)]
                    #:backend [backend (current-playback-backend)])
  (play-score (list v) legato #:tempo tempo #:backend backend))

(provide midi-event
         midi-event?
         midi-event-start
         midi-event-duration
         midi-event-pitch
         midi-event-velocity
         midi-event-channel
         midi-event-kind
         voice
         voice?
         voice-name
         voice-items
         expanded-voice
         expanded-voice?
         expanded-voice-name
         expanded-voice-items
         timed-item
         timed-item?
         timed-item-start
         timed-item-duration
         timed-item-object
         score-measure
         score-measure?
         score-measure-number
         score-measure-items
         measure
         flatten-items
         pedal-event
         pedal-event?
         pedal-event-start
         pedal-event-duration
         pedal-event-value
         pedal-event-channel
         midi-pedal
         midi-pedal?
         tie-start tie-end tie-continue
         tie-item? tie-base tie-item-from? tie-item-to?
         midi-rest
         midi-rest?
         midi-cluster
         midi-cluster?
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
         play-events
         play-score
         play-expanded-voice
         play-voice
         current-playback-backend
         current-playback-tempo
         current-soundfont)
