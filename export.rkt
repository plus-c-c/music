#lang racket

;; ============================================================
;; export.rkt — 导出/混缩
;; 类似 Ardour 的导出功能
;; ============================================================
;;
;; 功能:
;;   - 导出 MIDI 文件 (SMF)
;;   - 导出所有音轨到单一文件
;;   - 导出选定音轨
;;   - 导出混缩 (Mixdown)
;;   - 批量导出
;;

(require "midi-core.rkt"
         "midi-note.rkt"
         "midi-chord.rkt"
         "midi-perc.rkt"
         "midi-file.rkt"
         "mixer.rkt"
         "session.rkt")

(provide export-midi
         export-mixdown
         export-selected-tracks
         export-stems
         export-options%)

;; ============================================================
;; export-options% — 导出选项
;; ============================================================
(define export-options%
  (class object%
    (super-new)

    (init-field [format 1]            ; MIDI 格式: 0 或 1
                [ticks-per-beat 480]  ; 时间分辨率
                [tempo 120]
                [normalize? #f]       ; 是否标准化
                [include-cc? #t])     ; 是否包含 CC 消息

    (field [track-filter #f]  ; #f = 全部, 或 (listof track-name)
           [time-range #f])   ; #f = 全部, 或 (cons start-beat end-beat)

    (define/public (set-format! f) (set! format f) this)
    (define/public (set-ticks-per-beat! t) (set! ticks-per-beat t) this)
    (define/public (set-tempo! t) (set! tempo t) this)
    (define/public (set-normalize! n) (set! normalize? n) this)
    (define/public (set-include-cc! ic) (set! include-cc? ic) this)

    (define/public (set-track-filter! tracks)
      (set! track-filter (if (list? tracks) tracks #f))
      this)

    (define/public (set-time-range! start end)
      (set! time-range (cons start end))
      this)

    (define/public (include-track? name)
      (or (not track-filter) (member name track-filter)))
    ))

;; ============================================================
;; 导出函数
;; ============================================================

;; 导出整个会话为 MIDI 文件
(define (export-midi session filepath [options #f])
  (let ([opts (or options (new export-options%))]
        [file (new midi-file%
                   [format 1]
                   [ticks-per-beat 480])])
    ;; 设置元信息
    (send file add-track-name (send session get-name))
    (send file add-tempo (send session get-tempo))
    ;; 导出每个音轨
    (for ([track-name (send session get-track-names)])
      (when (send opts include-track? track-name)
        (let ([events (send session get-track track-name)])
          (add-events-to-file file events (send opts get-ticks-per-beat)))))
    (send file save filepath)
    (eprintf "MIDI 导出完成: ~a\n" filepath)
    file))

;; 导出混缩 (所有音轨混合到单一轨道)
(define (export-mixdown session filepath [options #f])
  (let ([opts (or options (new export-options% [format 0]))]
        [file (new midi-file%
                   [format 0]
                   [ticks-per-beat 480])])
    (send file add-track-name (string-append (send session get-name) " Mixdown"))
    (send file add-tempo (send session get-tempo))
    ;; 合并所有事件
    (let ([all-events '()])
      (for ([track-name (send session get-track-names)])
        (when (send opts include-track? track-name)
          (let ([events (send session get-track track-name)])
            (set! all-events (append all-events events)))))
      (add-events-to-file file all-events 480))
    (send file save filepath)
    (eprintf "混缩导出完成: ~a\n" filepath)
    file))

;; 导出选定音轨
(define (export-selected-tracks session filepath track-names [options #f])
  (let ([opts (or options (new export-options%))]
        [file (new midi-file%
                   [format 1]
                   [ticks-per-beat 480])])
    (send file add-track-name (send session get-name))
    (send file add-tempo (send session get-tempo))
    (for ([track-name track-names])
      (let ([events (send session get-track track-name)])
        (add-events-to-file file events 480)))
    (send file save filepath)
    (eprintf "选定音轨导出完成: ~a\n" filepath)
    file))

;; 批量导出每个音轨为单独文件 (Stems)
(define (export-stems session output-dir [options #f])
  (unless (directory-exists? output-dir)
    (make-directory* output-dir))
  (for ([track-name (send session get-track-names)])
    (let* ([filepath (build-path output-dir
                                 (format "~a.mid" track-name))]
           [file (new midi-file%
                      [format 0]
                      [ticks-per-beat 480])]
           [events (send session get-track track-name)])
      (send file add-track-name track-name)
      (send file add-tempo (send session get-tempo))
      (add-events-to-file file events 480)
      (send file save filepath)
      (eprintf "Stem 导出: ~a\n" track-name)))
  (eprintf "Stems 导出完成: ~a\n" output-dir))

;; ============================================================
;; 内部辅助
;; ============================================================

(define (add-events-to-file file events ticks-per-beat)
  (for-each (lambda (ev)
              (add-event-to-file file ev ticks-per-beat))
            events))

(define (add-event-to-file file event ticks-per-beat)
  (cond
    [(is-a? event midi-note%)
     (let* ([pitch (send event get-pitch)]
            [vel (send event get-velocity)]
            [dur (send event get-duration)]
            [ch (send event get-channel)]
            [dur-ticks (inexact->exact (round (* dur ticks-per-beat)))])
       (send file add-note pitch vel dur-ticks ch))]
    [(is-a? event midi-chord%)
     (for-each (lambda (n)
                 (let* ([pitch (send n get-pitch)]
                        [vel (send n get-velocity)]
                        [dur (send n get-duration)]
                        [ch (send n get-channel)]
                        [dur-ticks (inexact->exact (round (* dur ticks-per-beat)))])
                   (send file add-note pitch vel dur-ticks ch)))
               (send event get-notes))]
    [(is-a? event midi-perc%)
     (let* ([mn (send event get-midi-number)]
            [vel (send event get-velocity)]
            [dur (send event get-duration)]
            [dur-ticks (inexact->exact (round (* dur ticks-per-beat)))])
       (when mn
         (send file add-note mn vel dur-ticks 9)))]
    [(list? event)
     (for-each (lambda (e) (add-event-to-file file e ticks-per-beat)) event)]
    [else (void)]))
