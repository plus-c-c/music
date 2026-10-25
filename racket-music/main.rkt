#lang racket/base

;; #lang racket-music
;;
;; This language is Racket plus the local music DSL/environment.
;; The collection is intended to be linked locally:
;;   raco link --user /path/to/music/racket-music

(require (except-in racket eighth rest expand)
         (file "../demo/kaitou-lisp.rkt"))

(provide (all-from-out racket)
         (all-from-out (file "../demo/kaitou-lisp.rkt")))
