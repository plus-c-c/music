;;; music-scratch.el --- DrRacket-like scratch for the music DSL -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026
;; Keywords: languages, tools, music
;; Package-Requires: ((emacs "29.1") (racket-mode "20260926"))
;;
;;; Commentary:
;;
;; A persistent, file-backed scratch buffer for the music repository.
;;
;; It combines:
;;   - racket-mode as the definitions buffer;
;;   - racket-repl-mode as the interactions pane;
;;   - midi-lisp.rkt as the symbolic music environment.
;;
;; The buffer is intentionally close to Emacs' native *scratch*:
;;   C-j   evaluate the preceding expression and insert its value in-place
;;   C-c C-c run the scratch file, creating the Racket REPL
;;   C-c C-r send the active region to the REPL
;;   C-c C-z switch to the REPL
;;   C-c C-e show the music symbol at point
;;   C-c C-x expand the music symbol at point
;;   C-c C-p play the music symbol at point
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'racket-mode)
(require 'racket-repl)
(require 'racket-show)

(defgroup music-scratch nil
  "DrRacket-like scratch environment for the music DSL."
  :group 'languages
  :prefix "music-scratch-")

(defconst music-scratch--directory
  (file-name-directory
   (or load-file-name
       buffer-file-name
       (locate-library "music-scratch")))
  "Directory containing music-scratch.el.")

(defcustom music-scratch-repo-directory
  (expand-file-name ".." music-scratch--directory)
  "Root directory of the music repository."
  :type 'directory)

(defcustom music-scratch-file
  (locate-user-emacs-file "music-scratch.rkt")
  "Persistent backing file for the music scratch buffer."
  :type 'file)

(defcustom music-scratch-buffer-name "*Music Scratch*"
  "Name of the music scratch buffer."
  :type 'string)

(defcustom music-scratch-repl-buffer-name "*Music REPL*"
  "Name of the Racket REPL buffer used by the music scratch buffer."
  :type 'string)

(defvar-local music-scratch--inline-eval-p nil
  "Non-nil while a C-j result should be inserted into the scratch buffer.")

(defun music-scratch--starter-content ()
  "Return the initial content for `music-scratch-file'."
  (let ((midi (expand-file-name "midi-lisp.rkt" music-scratch-repo-directory))
        (kaitou (expand-file-name "demo/kaitou-lisp.rkt" music-scratch-repo-directory)))
    (format
     "#lang racket\n\
\n\
;; Music Scratch — C-c C-c run / C-c C-r send region\n\
;; C-j eval+print (void -> blank line; require/define go to REPL)\n\
;; C-c C-z REPL / C-c C-e show / C-c C-x expand / C-c C-p play\n\
\n\
(require (file %S)\n\
         (file %S))\n\
\n\
;; 这里的 define-voice 会同时定义变量并注册到 midi-lisp 符号环境。\n\
(define-voice scratch-voice\n\
  (part \"Scratch\"\n\
        (note 1/4 60 100 0)\n\
        (chord 1/4 62 'major 100 0)))\n\
\n\
;; 示例：\n\
;; (show 'scratch-voice)\n\
;; (play 'scratch-voice)\n\
;; (show 'melody)\n\
;; (play 'aria-intro)\n"
     midi kaitou)))

(defun music-scratch--ensure-file ()
  "Create the scratch file if necessary."
  (let ((dir (file-name-directory music-scratch-file)))
    (unless (file-exists-p dir)
      (make-directory dir t)))
  (unless (file-exists-p music-scratch-file)
    (with-temp-file music-scratch-file
      (insert (music-scratch--starter-content)))))

(defun music-scratch--insert-empty-line (pos)
  "Insert an empty result line at POS and move point to it."
  (goto-char (or pos (point)))
  (newline))

(defun music-scratch--show (string &optional pos)
  "Display STRING in-place for C-j, otherwise use racket-show.

A void or empty result inserts an empty line."
  (if music-scratch--inline-eval-p
      (progn
        (setq music-scratch--inline-eval-p nil)
        (if (or (null string)
                (string-empty-p string)
                (string= string "#<void>"))
            (music-scratch--insert-empty-line pos)
          (let ((text (replace-regexp-in-string "\n" "\n;;   " string)))
            (save-excursion
              (goto-char (or pos (point)))
              (insert " => " text)
              (newline)))))
    (racket-show-pseudo-tooltip string pos)))

(defun music-scratch--symbol-at-point ()
  "Return the music symbol at point, asking if necessary."
  (let ((symbol (thing-at-point 'symbol t)))
    (unless (and symbol (not (string-empty-p symbol)))
      (setq symbol (read-string "Music symbol: ")))
    (unless (and symbol (not (string-empty-p symbol)))
      (user-error "No music symbol"))
    symbol))

(defun music-scratch--send-string (string)
  "Submit STRING to the music REPL."
  (unless (racket--repl-session-id)
    (user-error "No Racket REPL session; run the scratch file first (C-c C-c)"))
  (let ((repl (get-buffer music-scratch-repl-buffer-name)))
    (unless (buffer-live-p repl)
      (user-error "No REPL buffer named %s" music-scratch-repl-buffer-name))
    (with-current-buffer repl
      (goto-char (point-max))
      (insert string)
      (racket-repl-submit))
    (display-buffer repl)))

(defun music-scratch--send-form (format-string value)
  "Send a one-line Racket form built from FORMAT-STRING and VALUE."
  (music-scratch--send-string
   (concat (format format-string value) "\n")))

(defun music-scratch--top-level-form-p (beg end)
  "Return non-nil if BEG..END starts a module-level definition."
  (let* ((text (string-trim (buffer-substring-no-properties beg end)))
         (head (when (string-match "\\`(\\([^ \t\n()]+\\)" text)
                 (match-string 1 text))))
    (and head
         (or (member head '("require" "provide" "module" "module*" "module+"
                            "struct" "begin"))
             (string-prefix-p "define" head)))))

;;;###autoload
(defun music-scratch ()
  "Open the persistent DrRacket-like music scratch buffer."
  (interactive)
  (music-scratch--ensure-file)
  (let ((buffer (find-file-noselect music-scratch-file)))
    (with-current-buffer buffer
      (rename-buffer music-scratch-buffer-name t)
      (unless (derived-mode-p 'racket-mode)
        (racket-mode))
      (setq-local racket-repl-buffer-name music-scratch-repl-buffer-name)
      (setq-local racket-show-functions '(music-scratch--show))
      (music-scratch-mode 1))
    (pop-to-buffer buffer)
    (message "Music scratch: C-c C-c run, C-j eval+print, C-c C-e show, C-c C-p play")))

(defun music-scratch-run ()
  "Run the scratch file and create/refresh the Racket REPL."
  (interactive)
  (unless (buffer-file-name)
    (user-error "Music scratch must be visiting `music-scratch-file'"))
  (racket-run)
  (message "Racket REPL starting; C-c C-z switches to it"))

(defun music-scratch-send-region (start end)
  "Send the active region to the Racket REPL."
  (interactive "r")
  (racket-send-region start end))

(defun music-scratch-switch-to-repl ()
  "Switch to the REPL associated with the music scratch buffer."
  (interactive)
  (let ((buffer (get-buffer music-scratch-repl-buffer-name)))
    (if (buffer-live-p buffer)
        (pop-to-buffer buffer)
      (user-error "No REPL yet; run the scratch file first (C-c C-c)"))))

(defun music-scratch-eval-print-last-sexp ()
  "Evaluate the preceding sexp like `C-j' in *scratch*.

Expressions are evaluated and their value is inserted after the sexp.
Module-level forms such as `require` and `define` are submitted to the
REPL instead, because they are not ordinary expressions."
  (interactive)
  (unless (racket--repl-session-id)
    (user-error "No Racket REPL session; run the scratch file first (C-c C-c)"))
  (let* ((beg (racket--start-of-previous-expression))
         (end (point)))
    (if (music-scratch--top-level-form-p beg end)
        (progn
          (racket-send-last-sexp)
          (music-scratch--insert-empty-line end))
      (setq music-scratch--inline-eval-p t)
      (condition-case err
          (racket-eval-last-sexp)
        (error
         (setq music-scratch--inline-eval-p nil)
         (signal (car err) (cdr err)))))))

(defun music-scratch-show-symbol-at-point ()
  "Show the expanded structure of the music symbol at point."
  (interactive)
  (music-scratch--send-form "(show '%s)" (music-scratch--symbol-at-point)))

(defun music-scratch-expand-symbol-at-point ()
  "Expand the music symbol at point in the REPL."
  (interactive)
  (music-scratch--send-form "(expand '%s)" (music-scratch--symbol-at-point)))

(defun music-scratch-play-symbol-at-point ()
  "Play the music symbol at point in a background Racket thread."
  (interactive)
  (music-scratch--send-form
   "(thread (lambda () (play '%s)))"
   (music-scratch--symbol-at-point))
  (message "Playback started in the music REPL"))

(defvar music-scratch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'music-scratch-run)
    (define-key map (kbd "C-c C-r") #'music-scratch-send-region)
    (define-key map (kbd "C-c C-z") #'music-scratch-switch-to-repl)
    (define-key map (kbd "C-c C-e") #'music-scratch-show-symbol-at-point)
    (define-key map (kbd "C-c C-x") #'music-scratch-expand-symbol-at-point)
    (define-key map (kbd "C-c C-p") #'music-scratch-play-symbol-at-point)
    (define-key map (kbd "C-j") #'music-scratch-eval-print-last-sexp)
    map)
  "Keymap for `music-scratch-mode'.")

(define-minor-mode music-scratch-mode
  "Minor mode for the persistent music scratch buffer."
  :lighter " MusicScratch"
  :keymap music-scratch-mode-map
  (when music-scratch-mode
    (setq-local racket-repl-buffer-name music-scratch-repl-buffer-name)
    (setq-local racket-show-functions '(music-scratch--show))))

(provide 'music-scratch)
;;; music-scratch.el ends here
