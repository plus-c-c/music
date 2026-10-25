#lang racket

;; Lisp 风格环境：符号解析、展开、展开前后结构统一播放输入。
(require "../midi-lisp.rkt"
         racket/port
         racket/string)

(define (assert v [msg "assertion failed"])
  (unless v (error 'lisp-test msg)))

(define env (make-music-environment))

(parameterize ([current-music-environment env])
  (define test-voice
    (part "Test"
          (note 1/4 60 100 0)
          (chord 1/4 62 'major 100 0)
          (rest 1/4)
          (pedal 1/4)))

  (define test-score
    (score
     test-voice
     (part "Counter"
           (rest 1/2)
           (note 1/2 48 80 1))))

  (bind-music! 'test-voice test-voice)
  (bind-music! "test-score" test-score)

  (assert (voice? (interpret 'test-voice)))
  (assert (voice? (interpret "test-voice")))
  (assert (= 2 (length (interpret 'test-score))))

  ;; 符号 -> 展开结构，保留 note/chord/rest/pedal。
  (define expanded (expand 'test-voice))
  (assert (expanded-voice? expanded))
  (assert (= 4 (length (expanded-voice-items expanded))))
  (assert (for/or ([ti (in-list (expanded-voice-items expanded))])
            (is-a? (timed-item-object ti) midi-chord%)))
  (assert (= 1 (length (expanded-voice-pedals expanded))))

  ;; 展开前的符号和展开后的结构得到相同的播放事件/踏板。
  (define-values (base-events base-pedals base-voices)
    (music->events 'test-voice))
  (define-values (expanded-events expanded-pedals expanded-voices)
    (music->events expanded))
  (assert (= base-voices expanded-voices))
  (assert (= (length base-events) (length expanded-events)))
  (assert (= (length base-pedals) (length expanded-pedals)))
  (assert (equal? (map midi-event-pitch base-events)
                  (map midi-event-pitch expanded-events)))

  ;; score 也能直接从符号或展开后的列表进入播放准备。
  (define-values (score-events-1 score-pedals-1 score-voices-1)
    (music->events 'test-score))
  (define-values (score-events-2 score-pedals-2 score-voices-2)
    (music->events (expand 'test-score)))
  (assert (= 2 score-voices-1 score-voices-2))
  (assert (= (length score-events-1) (length score-events-2)))
  (assert (= (length score-pedals-1) (length score-pedals-2)))

  ;; show 展示展开后的 note/chord。
  (define shown (format "~a" (show 'test-voice)))
  (assert (string-contains? shown "note C4"))
  (assert (string-contains? shown "chord"))
  (assert (string-contains? shown "pedal")))

(printf "PASS lisp env/symbol/expand/play-input\n")
