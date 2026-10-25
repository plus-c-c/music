#lang racket

;; ============================================================
;; NEST — Main Theme: default export (MIDI + WAV)
;;
;; 默认输出：
;;   demo/nest-theme.mid   SMF format 1, 480 PPQ
;;   demo/nest-theme.wav   48 kHz / 16-bit / stereo
;;
;; 用法：
;;   racket demo/export-nest-theme.rkt
;;   racket demo/export-nest-theme.rkt demo/my-theme
;;   racket demo/export-nest-theme.rkt --midi-only
;;   racket demo/export-nest-theme.rkt --no-master
;;   racket demo/export-nest-theme.rkt --soundfont /path/to/fluid.sf2
;; ============================================================

(require racket/string
         racket/path
         "../midi/main.rkt"
         "nest-theme.rkt")

;; ------------------------------------------------------------
;; 默认格式（要改默认值，只改这里）
;; ------------------------------------------------------------
(define default-base       "demo/nest-theme")
(define midi-format        1)      ; 0 = single-track, 1 = multi-track
(define midi-ppq           480)    ; ticks per quarter note
(define wav-sample-rate    48000)
(define wav-bit-depth      16)
(define wav-channels       2)
(define wav-gain-db        6)
(define wav-duration       84.4)   ; seconds, with tail
(define wav-fade-start     82.4)   ; seconds
(define default-soundfont  "/usr/share/soundfonts/FluidR3_GM.sf2")

(define (beat->tick b) (inexact->exact (round (* b midi-ppq))))

;; ------------------------------------------------------------
;; note list -> absolute-tick MIDI events
;; event = (tick priority status byte1 byte2)
;; priority: 0 = note-off, 1 = note-on (off first at same tick)
;; ------------------------------------------------------------
(define (collect-events notes start-beat)
  (let loop ([ns notes] [beat start-beat] [acc '()])
    (if (null? ns)
        acc
        (let* ([note (car ns)]
               [dur  (send note get-duration)]
               [s    (beat->tick beat)]
               [e    (beat->tick (+ beat dur))]
               [new
                (cond
                  [(is-a? note midi-chord%)
                   (append*
                    (for/list ([n (send note get-notes)])
                      (define p (send n get-pitch))
                      (define v (send n get-velocity))
                      (define ch (send n get-channel))
                      (list (list s 1 (+ #x90 ch) p v)
                            (list e 0 (+ #x80 ch) p 0))))]
                  [(is-a? note midi-perc%)
                   (define p (send note get-midi-number))
                   (define v (send note get-velocity))
                   (if p
                       (list (list s 1 #x99 p v)
                             (list e 0 #x89 p 0))
                       '())]
                  [else
                   (define p (send note get-pitch))
                   (define v (send note get-velocity))
                   (define ch (send note get-channel))
                   (list (list s 1 (+ #x90 ch) p v)
                         (list e 0 (+ #x80 ch) p 0))])])
          (loop (cdr ns) (+ beat dur) (append acc new))))))

(define (event<? a b)
  (or (< (list-ref a 0) (list-ref b 0))
      (and (= (list-ref a 0) (list-ref b 0))
           (< (list-ref a 1) (list-ref b 1)))))

(define (add-track mf name ch program events)
  (send mf new-track)
  (send mf add-track-name name)
  (when program
    (send mf add-program-change program ch))
  (send mf add-control-change 7 100 ch)
  (when (not (= ch 9))
    (send mf add-control-change 91 88 ch)
    (send mf add-control-change 93 36 ch))
  (let loop ([evs (sort events event<?)] [last 0])
    (unless (null? evs)
      (define ev (car evs))
      (send mf add-event (- (list-ref ev 0) last)
            (list-ref ev 2) (list-ref ev 3) (list-ref ev 4))
      (loop (cdr evs) (list-ref ev 0)))))

;; ------------------------------------------------------------
;; MIDI
;; ------------------------------------------------------------
(define (export-midi path)
  (define mf (make-midi-file midi-format midi-ppq tempo))

  ;; Conductor
  (send mf new-track)
  (send mf add-track-name "NEST Main Theme")
  (send mf add-tempo tempo)
  (send mf add-time-signature 4 4)
  (send mf add-key-signature 2 0)           ; D major, 2 sharps
  (send mf add-marker "end + reverb tail" (* 110 midi-ppq))

  ;; Instruments (channel, GM program, start beat)
  (add-track mf "Piano / melody"      0  0 (collect-events melody-notes 8))
  (add-track mf "Strings"             1 48 (collect-events string-notes 40))
  (add-track mf "Warm pad"            2 89 (collect-events pad-notes 0))
  (add-track mf "Contrabass / bass"   3 43 (collect-events bass-notes 8))
  (add-track mf "Harp / arps"         4 46 (collect-events arp-notes 8))
  (add-track mf "Tubular bells"       5 14 (collect-events bell-notes 72))
  (add-track mf "Tubular bells final" 5 14 (collect-events bell-final-notes 92))
  (add-track mf "Kick"                9 #f (collect-events kick-notes 40))
  (add-track mf "Cymbals"             9 #f (collect-events cymbal-notes 72))
  (add-track mf "Cymbals final"       9 #f (collect-events cymbal-final-notes 92))

  (send mf save path)
  (printf "MIDI: ~a  (SMF format ~a, ~a PPQ)\n" path midi-format midi-ppq)
  path)

;; ------------------------------------------------------------
;; WAV
;; ------------------------------------------------------------
(define (render-wav midi-path wav-path soundfont [no-master? #f])
  (define raw-path
    (path->string (path-replace-extension (string->path wav-path) #".raw.wav")))
  (define fluidsynth (find-executable-path "fluidsynth"))
  (unless fluidsynth
    (error 'render-wav "找不到 fluidsynth，请先安装 FluidSynth"))
  (define status
    (system* (path->string fluidsynth) "-i"
             "-T" "wav"
             "-O" (format "s~a" wav-bit-depth)
             "-r" (number->string wav-sample-rate)
             "-g" "0.7"
             "-F" raw-path
             soundfont midi-path))
  (unless status
    (error 'render-wav "fluidsynth 执行失败"))

  (define ffmpeg (find-executable-path "ffmpeg"))
  (cond
    [(and (not no-master?) ffmpeg)
     (define filter
       (format "volume=~adB,atrim=0:~a,afade=t=out:st=~a:d=2"
               wav-gain-db wav-duration wav-fade-start))
     (define st
       (system* ffmpeg "-y" "-hide_banner" "-loglevel" "error"
                "-i" raw-path
                "-af" filter
                "-ar" (number->string wav-sample-rate)
                "-ac" (number->string wav-channels)
                "-c:a" (format "pcm_s~ale" wav-bit-depth)
                wav-path))
     (unless st
       (error 'render-wav "ffmpeg 执行失败"))
     (delete-file raw-path)
     (printf "WAV : ~a  (~a Hz, ~a-bit, ~a ch, mastered)\n"
             wav-path wav-sample-rate wav-bit-depth wav-channels)]
    [else
     (rename-file-or-directory raw-path wav-path #t)
     (printf "WAV : ~a  (~a Hz, ~a-bit, ~a ch, raw)\n"
             wav-path wav-sample-rate wav-bit-depth wav-channels)]))

;; ------------------------------------------------------------
;; CLI / defaults
;; ------------------------------------------------------------
(define args (vector->list (current-command-line-arguments)))

(define (has-flag? f) (and (member f args) #t))

(define (option-value f)
  (let loop ([xs args])
    (cond
      [(null? xs) #f]
      [(equal? (car xs) f) (and (pair? (cdr xs)) (cadr xs))]
      [else (loop (cdr xs))])))

(define midi-only? (has-flag? "--midi-only"))
(define no-master? (has-flag? "--no-master"))

(define (strip-audio-extension s)
  (define e (path-get-extension (string->path s)))
  (if (and e (member e '(#".mid" #".midi" #".wav" #".wave")))
      (substring s 0 (- (string-length s) (bytes-length e)))
      s))

(define base-arg
  (let loop ([xs args])
    (cond
      [(null? xs) #f]
      [(member (car xs) '("--midi-only" "--no-master")) (loop (cdr xs))]
      [(equal? (car xs) "--soundfont") (loop (drop xs 2))]
      [(string-prefix? (car xs) "--") (loop (cdr xs))]
      [else (car xs)])))

(define base (strip-audio-extension (or base-arg default-base)))
(define midi-path (string-append base ".mid"))
(define wav-path  (string-append base ".wav"))

(define (ensure-parent path)
  (define parent (path-only (string->path path)))
  (when parent (make-directory* parent)))

(ensure-parent midi-path)
(export-midi midi-path)

(unless midi-only?
  (define soundfont
    (or (option-value "--soundfont")
        (getenv "NEST_SOUNDFONT")
        default-soundfont))
  (unless (file-exists? soundfont)
    (error 'export "找不到 SoundFont: ~a" soundfont))
  (ensure-parent wav-path)
  (render-wav midi-path wav-path soundfont no-master?))
