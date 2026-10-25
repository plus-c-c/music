#lang racket

;; ============================================================
;; midi-lisp.rkt — Lisp 风格的音乐环境
;;
;; 1. 一个环境保存“符号 -> voice/score/展开结构”的绑定；
;; 2. interpret 把符号解析成实际音乐对象；
;; 3. expand 把 voice/score 展开到 note/chord/rest/pedal；
;; 4. play 同时接受符号、展开前结构、展开后结构。
;;
;; 示例：
;;   (require "midi-lisp.rkt")
;;   (define-voice piano-high (part "Piano" (note 1/4 60)))
;;   (interpret 'piano-high)
;;   (expand 'piano-high)
;;   (play 'piano-high)
;;   (play (expand 'piano-high))
;; ============================================================

(require "midi/main.rkt"
         "midi-display.rkt"
         (only-in racket/list append*)
         (only-in racket/port with-output-to-string))

(provide (all-from-out "midi/main.rkt")
         current-music-environment
         make-music-environment
         bind-music!
         lookup-music
         interpret
         define-music
         define-voice
         define-score
         expand
         music->events
         show
         show->string
         play)

;; ------------------------------------------------------------
;; 符号环境
;; ------------------------------------------------------------
(define (make-music-environment)
  (make-hash))

(define current-music-environment
  (make-parameter (make-music-environment)))

(define (normalize-music-symbol value)
  (cond
    [(symbol? value) value]
    [(string? value) (string->symbol value)]
    [else
     (error 'music-environment "声部名必须是符号或字符串，收到: ~a" value)]))

(define (bind-music! name value [env (current-music-environment)])
  (hash-set! env (normalize-music-symbol name) value)
  value)

(define (lookup-music name [env (current-music-environment)])
  (hash-ref env (normalize-music-symbol name)
            (lambda ()
              (error 'interpret "环境里没有声部/乐谱: ~a" name))))

(define (interpret value [env (current-music-environment)])
  (cond
    [(or (symbol? value) (string? value))
     (lookup-music value env)]
    [else value]))

;; REPL 中定义并自动注册：
;;   (define-voice piano-high (part ...))
;;   (define-score aria-intro (score ...))
(define-syntax-rule (define-music name value)
  (define name
    (let ([music-value value])
      (bind-music! 'name music-value)
      music-value)))

(define-syntax-rule (define-voice name value)
  (define-music name value))

(define-syntax-rule (define-score name value)
  (define-music name value))

;; ------------------------------------------------------------
;; 展开
;; ------------------------------------------------------------
(define (expand value [env (current-music-environment)])
  (define resolved (interpret value env))
  (cond
    [(voice? resolved)
     (car (expand-score (list resolved)))]
    [(expanded-voice? resolved)
     resolved]
    [(midi-event? resolved)
     (list resolved)]
    [(list? resolved)
     (cond
       [(null? resolved) '()]
       [(andmap voice? resolved) (expand-score resolved)]
       [(andmap expanded-voice? resolved) resolved]
       [(andmap midi-event? resolved) resolved]
       [else
        (error 'expand "列表中必须全是 voice、expanded-voice 或 midi-event: ~a"
               resolved)])]
    [else
     (error 'expand "无法展开: ~a" resolved)]))

;; 展开前后的音乐对象 -> 播放事件 + 踏板 + 声部数。
(define (music->events value
                        #:legato [legato 1.0]
                        #:env [env (current-music-environment)])
  (define resolved (interpret value env))
  (cond
    [(voice? resolved)
     (values (score-events (list resolved) legato)
             (score-pedals (list resolved))
             1)]
    [(expanded-voice? resolved)
     (values (expanded-voice-events resolved legato)
             (expanded-voice-pedals resolved)
             1)]
    [(midi-event? resolved)
     (values (list resolved) '() 1)]
    [(list? resolved)
     (cond
       [(null? resolved) (values '() '() 0)]
       [(andmap voice? resolved)
        (values (score-events resolved legato)
                (score-pedals resolved)
                (length resolved))]
       [(andmap expanded-voice? resolved)
        (values (append* (for/list ([ev (in-list resolved)])
                           (expanded-voice-events ev legato)))
                (append* (for/list ([ev (in-list resolved)])
                           (expanded-voice-pedals ev)))
                (length resolved))]
       [(andmap midi-event? resolved)
        (values resolved '() 1)]
       [else
        (error 'music->events
               "列表中必须全是 voice、expanded-voice 或 midi-event: ~a"
               resolved)])]
    [else
     (error 'music->events "无法播放: ~a" resolved)]))

;; ------------------------------------------------------------
;; 显示
;;
;; `show` returns a plain Racket string.  C-j / the REPL therefore print
;; it as a legal Racket string, with double quotes and escaped newlines.
;; `show->string` is the same pure text API; `show` is kept as the
;; display-oriented name.
;; ------------------------------------------------------------
(define (drop-final-newline str)
  (if (and (positive? (string-length str))
           (char=? (string-ref str (sub1 (string-length str))) #\newline))
      (substring str 0 (sub1 (string-length str)))
      str))

(define (show-expanded-voice ev)
  (printf "~a\n" (expanded-voice-name ev))
  (for ([ti (in-list (expanded-voice-items ev))])
    (printf "  ~a  +~a  ~a\n"
            (timed-item-start ti)
            (timed-item-duration ti)
            (object->notation (timed-item-object ti)))))

(define (show-event e)
  (printf "  ~a  +~a  ~a  ch~a\n"
          (midi-event-start e)
          (midi-event-duration e)
          (pitch-name (midi-event-pitch e))
          (midi-event-channel e)))

(define (show->string value [env (current-music-environment)])
  (define expanded (expand value env))
  (define text
    (with-output-to-string
      (lambda ()
        (cond
          [(expanded-voice? expanded)
           (show-expanded-voice expanded)]
          [(list? expanded)
           (cond
             [(null? expanded) (printf "（空）\n")]
             [(andmap expanded-voice? expanded)
              (for ([ev (in-list expanded)])
                (show-expanded-voice ev)
                (newline))]
             [(andmap midi-event? expanded)
              (for ([e (in-list expanded)])
                (show-event e))]
             [else
              (error 'show "无法显示: ~a" expanded)])]
          [else
           (error 'show "无法显示: ~a" expanded)]))))
  (drop-final-newline text))

(define (show value [env (current-music-environment)])
  (show->string value env))

;; ------------------------------------------------------------
;; 播放
;; ------------------------------------------------------------
(define (play value
              #:tempo [tempo (current-playback-tempo)]
              #:backend [backend (current-playback-backend)]
              #:legato [legato 1.0]
              #:env [env (current-music-environment)])
  (define-values (events pedals voice-count)
    (music->events value #:legato legato #:env env))
  (play-events events voice-count
               #:tempo tempo
               #:backend backend
               #:pedals pedals)
  (void))
