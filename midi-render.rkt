#lang racket

;; ============================================================
;; midi-render.rkt — 按拍区间渲染 / 切分 / 播放音频
;;
;; 功能：
;;   - 从 score/voice 事件中截取一段拍区间；
;;   - 写临时 MIDI，用 FluidSynth 渲染 WAV；
;;   - 从原始音频用 ffmpeg 截取同一区间；
;;   - 前台或后台播放 WAV。
;; ============================================================

(require "midi-score.rkt"
         "midi-file.rkt"
         "midi-core.rkt"
         "pretty-midi.rkt"
         racket/system
         racket/file
         racket/path
         racket/string)

;; ------------------------------------------------------------
;; 事件区间
;; ------------------------------------------------------------
(define (events-in-beat-range evs start end)
  (for/list ([e (in-list evs)]
             #:when (and (< (midi-event-start e) end)
                         (> (+ (midi-event-start e) (midi-event-duration e)) start)))
    (define s (max start (midi-event-start e)))
    (define e* (min end (+ (midi-event-start e) (midi-event-duration e))))
    (midi-event s (- e* s)
                (midi-event-pitch e)
                (midi-event-velocity e)
                (midi-event-channel e)
                (midi-event-kind e))))

;; ------------------------------------------------------------
;; 事件 -> MIDI
;; ------------------------------------------------------------
(define (beat->tick b [ppq 480])
  (inexact->exact (round (* b ppq))))

(define (write-events-midi evs path tempo)
  (define ppq 480)
  (define mf (make-midi-file 0 ppq tempo))
  (send mf add-track-name "Render")
  (send mf add-tempo tempo)
  (for ([ch (remove-duplicates (map midi-event-channel evs))])
    (unless (= ch 9)
      (send mf add-program-change 0 ch)))
  (define msgs
    (sort
     (append*
      (for/list ([e evs])
        (define ch (midi-event-channel e))
        (define p (midi-event-pitch e))
        (define s (beat->tick (midi-event-start e) ppq))
        (define e* (beat->tick (+ (midi-event-start e)
                                  (midi-event-duration e))
                               ppq))
        (list (list s 2 (+ #x90 ch) p (midi-event-velocity e))
              (list e* 1 (+ #x80 ch) p 0))))
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

;; ------------------------------------------------------------
;; 渲染 WAV
;; ------------------------------------------------------------
(define (path-string p)
  (if (path? p) (path->string p) p))

(define (render-midi-to-wav midi-path wav-path)
  (define fluidsynth (find-executable-path "fluidsynth"))
  (unless fluidsynth
    (error 'midi-render "找不到 fluidsynth"))
  (define soundfont (current-soundfont))
  (unless (file-exists? soundfont)
    (error 'midi-render "找不到 SoundFont: ~a" soundfont))
  (define ok?
    (system* (path->string fluidsynth)
             "-q" "-ni" "-a" "file"
             "-F" (path-string wav-path)
             "-r" "44100"
             soundfont
             (path-string midi-path)))
  (unless ok? (error 'midi-render "FluidSynth 渲染失败")))

(define (render-events-wav evs path tempo)
  (define mid (make-temporary-file "kaitou-render-~a.mid"))
  (dynamic-wind
    void
    (lambda ()
      (write-events-midi evs mid tempo)
      (render-midi-to-wav mid path))
    (lambda ()
      (when (file-exists? mid) (delete-file mid)))))

(define (ensure-parent path)
  (define parent (path-only (string->path path)))
  (when parent (make-directory* parent)))

(define (render-score-range-wav score start end path tempo)
  (ensure-parent path)
  (render-events-wav
   (events-in-beat-range (score-events score) start end)
   path tempo))

(define (render-voice-range-wav v start end path tempo)
  (ensure-parent path)
  (render-events-wav
   (events-in-beat-range (voice-events v) start end)
   path tempo))

;; 把渲染结果裁到指定时长，去掉 FluidSynth 尾音
(define (trim-wav input output duration-second)
  (ensure-parent output)
  (define ffmpeg (find-executable-path "ffmpeg"))
  (unless ffmpeg (error 'midi-render "找不到 ffmpeg"))
  (define ok?
    (system* (path->string ffmpeg)
             "-loglevel" "error"
             "-y" "-i" input
             "-af" "apad"
             "-t" (number->string duration-second)
             "-ar" "44100" "-ac" "2"
             output))
  (unless ok? (error 'midi-render "ffmpeg 裁剪失败")))

;; ------------------------------------------------------------
;; 原始音频截取
;; ------------------------------------------------------------
(define (render-audio-range-wav input start-second duration-second path)
  (ensure-parent path)
  (define ffmpeg (find-executable-path "ffmpeg"))
  (unless ffmpeg (error 'midi-render "找不到 ffmpeg"))
  (define ok?
    (system* (path->string ffmpeg)
             "-loglevel" "error"
             "-y"
             "-i" input
             "-ss" (number->string start-second)
             "-t" (number->string duration-second)
             "-ar" "44100" "-ac" "2"
             path))
  (unless ok? (error 'midi-render "ffmpeg 截取失败")))

;; ------------------------------------------------------------
;; 播放
;; ------------------------------------------------------------
(define (player-command path)
  (define path* (path-string path))
  (cond
    [(find-executable-path "paplay")
     => (lambda (p) (list (path->string p) path*))]
    [(find-executable-path "ffplay")
     => (lambda (p) (list (path->string p) "-nodisp" "-autoexit"
                          "-loglevel" "quiet" path*))]
    [(find-executable-path "aplay")
     => (lambda (p) (list (path->string p) "-q" path*))]
    [else (error 'midi-render "找不到音频播放器")]))

(define (shell-quote s)
  (string-append "'" (string-replace s "'" "'\\''") "'"))

(define (play-wav path)
  (define cmd (player-command path))
  (unless (apply system* cmd)
    (error 'midi-render "播放失败: ~a" path)))

(define (play-wav-background path)
  (define cmd (player-command path))
  (define line
    (format "~a >/dev/null 2>&1 &"
            (string-join (map shell-quote cmd) " ")))
  (subprocess #f #f #f "/bin/sh" "-c" line))

(define (play-wav-list-background paths)
  (when (pair? paths)
    (define lines
      (for/list ([p (in-list paths)])
        (format "~a; sleep 0.2;"
                (string-join (map shell-quote (player-command p)) " "))))
    (subprocess #f #f #f "/bin/sh" "-c"
                (string-append (string-join lines "\n")
                               "\nwait\n"))))

(provide events-in-beat-range
         write-events-midi
         render-midi-to-wav
         render-events-wav
         render-score-range-wav
         render-voice-range-wav
         render-audio-range-wav
         trim-wav
         player-command
         play-wav
         play-wav-background
         play-wav-list-background)
