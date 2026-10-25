#lang racket

;; ============================================================
;; session.rkt — 会话保存/加载
;; 类似 Ardour 的会话管理
;; ============================================================
;;
;; 功能:
;;   - 保存完整会话到 JSON
;;   - 加载会话
;;   - 导出/导入音轨
;;   - 撤销/重做历史
;;

(require json
         racket/port
         "midi-core.rkt"
         "midi-note.rkt"
         "midi-chord.rkt"
         "midi-perc.rkt"
         "mixer.rkt")

(provide session%
         make-session
         session-save
         session-load)

(define session%
  (class object%
    (super-new)

    (init-field [name "Untitled"]
                [filepath #f])

    (field [tempo 120]
           [beats-per-measure 4]
           [beat-subdivision 4]
           [tracks '()]           ; ((name . events) ...)
           [mixer-settings #f]    ; mixer% 对象
           [markers (make-hash)]  ; name → beat
           [metadata (make-hash)] ; 任意元数据
           [undo-stack '()]
           [redo-stack '()])

    ;; ---- 基本属性 ----

    (define/public (get-name) name)
    (define/public (set-name! n) (set! name n) this)
    (define/public (get-tempo) tempo)
    (define/public (set-tempo! t) (set! tempo t) this)
    (define/public (set-time-signature! beats subdivision)
      (set! beats-per-measure beats)
      (set! beat-subdivision subdivision)
      this)

    ;; ---- 音轨管理 ----

    (define/public (add-track! name events)
      (push-undo!)
      (set! tracks (cons (cons name (events->serializable events)) tracks))
      this)

    (define/public (remove-track! name)
      (push-undo!)
      (set! tracks (filter (lambda (t) (not (string=? (car t) name))) tracks))
      this)

    (define/public (get-track name)
      (let ([t (assoc name tracks)])
        (if t (serializable->events (cdr t)) '())))

    (define/public (get-track-names)
      (map car tracks))

    (define/public (update-track! name events)
      (push-undo!)
      (let ([t (assoc name tracks)])
        (when t
          (set! tracks (map (lambda (tr)
                              (if (string=? (car tr) name)
                                  (cons name (events->serializable events))
                                  tr))
                            tracks))))
      this)

    ;; ---- 标记 ----

    (define/public (set-marker! name beat)
      (hash-set! markers name beat)
      this)

    (define/public (get-marker name)
      (hash-ref markers name #f))

    (define/public (get-markers) markers)

    ;; ---- 元数据 ----

    (define/public (set-meta! key value)
      (hash-set! metadata key value)
      this)

    (define/public (get-meta key)
      (hash-ref metadata key #f))

    ;; ---- 混音器 ----

    (define/public (set-mixer! mx)
      (set! mixer-settings mx)
      this)

    (define/public (get-mixer) mixer-settings)

    ;; ---- 保存 ----

    (define/public (save [path #f])
      (when path (set! filepath path))
      (unless filepath
        (error 'session "未指定文件路径"))
      (let* ([data (to-hash)]
             [json-str (jsexpr->string data)]
             [out (open-output-file filepath #:exists 'replace)])
        (display json-str out)
        (close-output-port out)
        (eprintf "会话已保存: ~a\n" filepath))
      this)

    ;; ---- 加载 ----

    (define/public (load! [path #f])
      (when path (set! filepath path))
      (unless filepath
        (error 'session "未指定文件路径"))
      (unless (file-exists? filepath)
        (error 'session "文件不存在: ~a" filepath))
      (let* ([in (open-input-file filepath)]
             [json-str (port->string in)]
             [data (string->jsexpr json-str)])
        (from-hash data)
        (close-input-port in)
        (eprintf "会话已加载: ~a\n" filepath))
      this)

    ;; ---- 撤销/重做 ----

    (define/public (undo)
      (when (not (null? undo-stack))
        (set! redo-stack (cons (snapshot) redo-stack))
        (restore! (car undo-stack))
        (set! undo-stack (cdr undo-stack)))
      this)

    (define/public (redo)
      (when (not (null? redo-stack))
        (set! undo-stack (cons (snapshot) undo-stack))
        (restore! (car redo-stack))
        (set! redo-stack (cdr redo-stack)))
      this)

    (define/private (push-undo!)
      (set! undo-stack (cons (snapshot) undo-stack))
      (when (> (length undo-stack) 100)
        (set! undo-stack (take undo-stack 100)))
      (set! redo-stack '()))

    (define/private (snapshot)
      (hash 'name name
            'tempo tempo
            'bpm beats-per-measure
            'bsd beat-subdivision
            'tracks (copy-track-list tracks)
            'markers (hash-copy markers)))

    (define/private (restore! snap)
      (set! name (hash-ref snap 'name))
      (set! tempo (hash-ref snap 'tempo))
      (set! beats-per-measure (hash-ref snap 'bpm))
      (set! beat-subdivision (hash-ref snap 'bsd))
      (set! tracks (copy-track-list (hash-ref snap 'tracks)))
      (set! markers (hash-ref snap 'markers)))

    (define (copy-track-list tl)
      (map (lambda (t) (cons (car t) (cdr t))) tl))

    ;; ---- 序列化 ----

    (define/public (to-hash)
      (hash 'name name
            'tempo tempo
            'beats-per-measure beats-per-measure
            'beat-subdivision beat-subdivision
            'tracks (map (lambda (t)
                           (hash 'name (car t)
                                 'events (cdr t)))
                         tracks)
            'markers (for/hash ([(k v) markers])
                       (values k v))
            'metadata (for/hash ([(k v) metadata])
                        (values k v))))

    (define/public (from-hash data)
      (set! name (hash-ref data 'name "Untitled"))
      (set! tempo (hash-ref data 'tempo 120))
      (set! beats-per-measure (hash-ref data 'beats-per-measure 4))
      (set! beat-subdivision (hash-ref data 'beat-subdivision 4))
      (set! tracks
            (map (lambda (t)
                   (cons (hash-ref t 'name)
                         (hash-ref t 'events)))
                 (hash-ref data 'tracks '())))
      (set! markers
            (if (hash-has-key? data 'markers)
                (hash-ref data 'markers)
                (make-hash)))
      (set! metadata
            (if (hash-has-key? data 'metadata)
                (hash-ref data 'metadata)
                (make-hash)))
      this)

    ;; ---- 导出事件 ----

    (define/public (export-events track-name)
      (serializable->events (or (cdr (assoc track-name tracks)) '())))
    ))

;; ============================================================
;; 事件序列化
;; ============================================================

;; 将事件列表转为可序列化的 JSON 格式
(define (events->serializable events)
  (map event->serializable events))

(define (event->serializable event)
  (cond
    [(is-a? event midi-note%)
     (hash 'type 'note
           'pitch (send event get-pitch)
           'velocity (send event get-velocity)
           'duration (send event get-duration)
           'channel (send event get-channel))]
    [(is-a? event midi-chord%)
     (hash 'type 'chord
           'root (send event get-root)
           'chord-type (symbol->string (send event get-type))
           'duration (send event get-duration)
           'velocity (send event get-velocity)
           'channel (send event get-channel))]
    [(is-a? event midi-perc%)
     (hash 'type 'perc
           'drum (symbol->string (send event get-drum))
           'velocity (send event get-velocity)
           'duration (send event get-duration))]
    [(list? event) (map event->serializable event)]
    [else event]))

;; 从序列化格式恢复事件
(define (serializable->events data)
  (map serializable->event data))

(define (serializable->event data)
  (cond
    [(hash? data)
     (case (hash-ref data 'type)
       [(note) (midi-note (hash-ref data 'pitch)
                          (hash-ref data 'velocity)
                          (hash-ref data 'duration)
                          (hash-ref data 'channel))]
       [(chord) (midi-chord (hash-ref data 'root)
                            (string->symbol (hash-ref data 'chord-type))
                            (hash-ref data 'duration)
                            (hash-ref data 'velocity)
                            (hash-ref data 'channel))]
       [(perc) (midi-perc (string->symbol (hash-ref data 'drum))
                          (hash-ref data 'velocity)
                          (hash-ref data 'duration))]
       [else data])]
    [(list? data) (map serializable->event data)]
    [else data]))

;; ============================================================
;; 便捷函数
;; ============================================================

(define (make-session [name "Untitled"])
  (new session% [name name]))

(define (session-save session filepath)
  (send session save filepath))

(define (session-load filepath)
  (let ([s (new session%)])
    (send s load! filepath)
    s))
