#lang racket/base

;; RtMidi FFI Wrapper for Racket
;; Binds rtmidi_c.h C interface directly via Racket FFI

(require ffi/unsafe
         ffi/unsafe/define
         racket/runtime-path
         racket/class
         racket/base)

;; ==================== Library Loading ====================

(define-runtime-path this-dir ".")
(define librtmidi
  (ffi-lib (build-path this-dir "librtmidi")))

(define-ffi-definer define-rtmidi librtmidi)

;; ==================== Constants ====================

(define RTMIDI_API_UNSPECIFIED  0)
(define RTMIDI_API_MACOSX_CORE  1)
(define RTMIDI_API_LINUX_ALSA   2)
(define RTMIDI_API_UNIX_JACK    3)
(define RTMIDI_API_WINDOWS_MM   4)
(define RTMIDI_API_RTMIDI_DUMMY 5)

;; ==================== Raw C Bindings ====================

(define _RtMidiPtr (_cpointer 'RtMidiWrapper))

(define-rtmidi rtmidi_get_version
  (_fun -> _string/utf-8))

(define-rtmidi rtmidi_open_port
  (_fun _RtMidiPtr _uint _string/utf-8 -> _void))

(define-rtmidi rtmidi_close_port
  (_fun _RtMidiPtr -> _void))

(define-rtmidi rtmidi_get_port_count
  (_fun _RtMidiPtr -> _uint))

;; int rtmidi_get_port_name(RtMidiPtr device, unsigned int portNumber,
;;                          char *bufOut, int *bufLen)
(define-rtmidi rtmidi_get_port_name
  (_fun _RtMidiPtr _uint
        _pointer       ; bufOut (char*)
        _pointer       ; bufLen (int*) - caller manages memory
        -> _int))

(define-rtmidi rtmidi_out_create_default
  (_fun -> _RtMidiPtr))

(define-rtmidi rtmidi_out_create
  (_fun _int _string/utf-8 -> _RtMidiPtr))

(define-rtmidi rtmidi_out_free
  (_fun _RtMidiPtr -> _void))

;; int rtmidi_out_send_message(RtMidiOutPtr, const unsigned char *message, int length)
(define-rtmidi rtmidi_out_send_message
  (_fun _RtMidiPtr _pointer _int -> _int))

(define-rtmidi rtmidi_in_create_default
  (_fun -> _RtMidiPtr))

(define-rtmidi rtmidi_in_create
  (_fun _int _string/utf-8 _uint -> _RtMidiPtr))

(define-rtmidi rtmidi_in_free
  (_fun _RtMidiPtr -> _void))

;; double rtmidi_in_get_message(RtMidiInPtr, unsigned char *message, size_t *size)
(define-rtmidi rtmidi_in_get_message
  (_fun _RtMidiPtr
        _pointer                ; message (unsigned char*)
        (_ptr io _uint)         ; size (size_t*)
        -> _double))

(define-rtmidi rtmidi_in_ignore_types
  (_fun _RtMidiPtr _bool _bool _bool -> _void))

;; ==================== Port Name Helper ====================

(define (rtmidi-port-name device idx)
  (define buf-size 512)
  (define buf (malloc _byte buf-size))
  (memset buf 0 buf-size)
  (define len-ptr (malloc _int 1))
  (ptr-set! len-ptr _int buf-size)
  (define result (rtmidi_get_port_name device idx buf len-ptr))
  (if (>= result 0)
      (cast buf _pointer _string/utf-8)
      ""))

;; ==================== Racket High-Level API ====================

(define rtmidi-out%
  (class object%
    (super-new)
    (field [handle #f])

    (define/public (open [port-idx 0] [name "Racket MIDI"])
      (set! handle (rtmidi_out_create RTMIDI_API_UNSPECIFIED name))
      (define count (rtmidi_get_port_count handle))
      (when (<= count 0)
        (error 'rtmidi-out "No MIDI output ports found"))
      (when (>= port-idx count)
        (error 'rtmidi-out (format "Port ~a not found (~a ports)" port-idx count)))
      (rtmidi_open_port handle port-idx name)
      (void))

    (define/public (close)
      (when handle
        (rtmidi_close_port handle)
        (rtmidi_out_free handle)
        (set! handle #f)))

    (define/public (port-count)
      (if handle (rtmidi_get_port_count handle) 0))

    (define/public (port-name idx)
      (unless handle (error 'rtmidi-out "Not open"))
      (rtmidi-port-name handle idx))

    (define/public (list-ports)
      (for/list ([i (in-range (port-count))])
        (cons i (port-name i))))

    (define/public (send-message msg)
      (unless handle (error 'rtmidi-out "Not open"))
      (define bs
        (cond
          [(list? msg) (list->bytes msg)]
          [(bytes? msg) msg]
          [else (error 'rtmidi-out "Expected bytes or list")]))
      (define buf (malloc _byte (bytes-length bs)))
      (memcpy buf bs (bytes-length bs))
      (define result (rtmidi_out_send_message handle buf (bytes-length bs)))
      (unless (= result 0)
        (error 'rtmidi-out "Send failed"))
      (void))

    (define/public (note-on pitch [vel 100] [ch 0])
      (send-message (list (+ #x90 ch) (bitwise-and pitch #x7F) (bitwise-and vel #x7F))))

    (define/public (note-off pitch [vel 0] [ch 0])
      (send-message (list (+ #x80 ch) (bitwise-and pitch #x7F) (bitwise-and vel #x7F))))

    (define/public (cc ctrl val [ch 0])
      (send-message (list (+ #xB0 ch) (bitwise-and ctrl #x7F) (bitwise-and val #x7F))))

    (define/public (pc prog [ch 0])
      (send-message (list (+ #xC0 ch) (bitwise-and prog #x7F))))

    (define/public (pitch-bend val [ch 0])
      (define clamped (bitwise-and val #x3FFF))
      (send-message (list (+ #xE0 ch)
                          (bitwise-and clamped #x7F)
                          (bitwise-and (arithmetic-shift clamped -7) #x7F))))))

(define rtmidi-in%
  (class object%
    (super-new)
    (field [handle #f]
           [msg-buf (malloc _byte 1024)])

    (define/public (open [port-idx 0] [name "Racket MIDI In"])
      (set! handle (rtmidi_in_create RTMIDI_API_UNSPECIFIED name 1024))
      (define count (rtmidi_get_port_count handle))
      (when (<= count 0)
        (error 'rtmidi-in "No MIDI input ports found"))
      (when (>= port-idx count)
        (error 'rtmidi-in (format "Port ~a not found (~a ports)" port-idx count)))
      (rtmidi_open_port handle port-idx name)
      (void))

    (define/public (close)
      (when handle
        (rtmidi_close_port handle)
        (rtmidi_in_free handle)
        (set! handle #f)))

    (define/public (port-count)
      (if handle (rtmidi_get_port_count handle) 0))

    (define/public (port-name idx)
      (unless handle (error 'rtmidi-in "Not open"))
      (rtmidi-port-name handle idx))

    (define/public (list-ports)
      (for/list ([i (in-range (port-count))])
        (cons i (port-name i))))

    (define/public (get-message)
      (unless handle (error 'rtmidi-in "Not open"))
      (define size-ptr (malloc _int 1))
      (ptr-set! size-ptr _uint 1024)
      (define timestamp (rtmidi_in_get_message handle msg-buf size-ptr))
      (define size (ptr-ref size-ptr _uint))
      (if (> size 0)
          (cast msg-buf _pointer _bytes)
          #f))))

;; ==================== Convenience Functions ====================

(define (make-rtmidi-out [port-idx 0] [name "Racket MIDI"])
  (define dev (new rtmidi-out%))
  (send dev open port-idx name)
  dev)

(define (make-rtmidi-in [port-idx 0] [name "Racket MIDI In"])
  (define dev (new rtmidi-in%))
  (send dev open port-idx name)
  dev)

(define (list-midi-out-ports)
  (define dev (new rtmidi-out%))
  (set-field! handle dev (rtmidi_out_create RTMIDI_API_UNSPECIFIED "probe"))
  (define ports
    (for/list ([i (in-range (send dev port-count))])
      (cons i (send dev port-name i))))
  (send dev close)
  ports)

(define (list-midi-in-ports)
  (define dev (new rtmidi-in%))
  (set-field! handle dev (rtmidi_in_create RTMIDI_API_UNSPECIFIED "probe" 1024))
  (define ports
    (for/list ([i (in-range (send dev port-count))])
      (cons i (send dev port-name i))))
  (send dev close)
  ports)

;; ==================== Provide ====================

(provide
 rtmidi-out% rtmidi-in%
 make-rtmidi-out make-rtmidi-in
 list-midi-out-ports list-midi-in-ports
 RTMIDI_API_UNSPECIFIED RTMIDI_API_LINUX_ALSA RTMIDI_API_UNIX_JACK)
