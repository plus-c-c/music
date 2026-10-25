#lang racket/base

;; #lang racket-music
;;
;; The language provides Racket plus the music DSL/environment.
;; Demo scores are deliberately NOT part of the language; import them
;; separately, for example:
;;
;;   (require racket-music/kaitou)
;;
;; The collection is intended to be linked locally:
;;   raco link --user /path/to/music/racket-music

(require (except-in racket eighth rest expand)
         (file "../midi-lisp.rkt"))

(provide (all-from-out racket)
         (all-from-out (file "../midi-lisp.rkt")))
