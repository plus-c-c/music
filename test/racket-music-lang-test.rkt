#lang racket-music

;; #lang racket-music 应同时提供 Racket 与本地音乐环境。
(require rackunit
         racket/port
         racket-music/kaitou)

(check-true (voice? (interpret 'piano-high)))
(check-true (expanded-voice? (expand 'melody)))
(check-equal? (expanded-voice-name (expand 'melody)) "Melody")
(check-true (string? (with-output-to-string (lambda () (show 'melody)))))

(printf "PASS #lang racket-music\n")
