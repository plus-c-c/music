#lang racket

;; MIDI Core - 基础常量和工具函数

;; MIDI 消息类型
(define NOTE-OFF #x80)
(define NOTE-ON #x90)
(define POLY-PRESSURE #xA0)
(define CONTROL-CHANGE #xB0)
(define PROGRAM-CHANGE #xC0)
(define CHANNEL-PRESSURE #xD0)
(define PITCH-BEND #xE0)

;; 系统消息
(define META-STATUS #xFF)
(define SYSEX-START #xF0)
(define SYSEX-END #xF7)

;; General MIDI 打击乐映射 (Channel 9)
(define drum-map
  (hash
   'acoustic-bass-drum 35
   'bass-drum-1 36
   'side-stick 37
   'acoustic-snare 38
   'hand-clap 39
   'electric-snare 40
   'low-floor-tom 41
   'closed-hi-hat 42
   'high-floor-tom 43
   'pedal-hi-hat 44
   'low-tom 45
   'open-hi-hat 46
   'low-mid-tom 47
   'hi-mid-tom 48
   'crash-cymbal-1 49
   'high-tom 50
   'ride-cymbal-1 51
   'chinese-cymbal 52
   'ride-bell 53
   'tambourine 54
   'splash-cymbal 55
   'cowbell 56
   'crash-cymbal-2 57
   'vibraslap 58
   'ride-cymbal-2 59
   'hi-bongo 60
   'low-bongo 61
   'mute-hi-conga 62
   'open-hi-conga 63
   'low-conga 64
   'high-timbale 65
   'low-timbale 66
   'high-agogo 67
   'low-agogo 68
   'cabasa 69
   'maracas 70
   'short-whistle 71
   'long-whistle 72
   'guiro 73
   'claves 74
   'hi-wood-block 75
   'low-wood-block 76
   'mute-cuica 77
   'open-cuica 78
   'mute-triangle 79
   'open-triangle 80))

;; General MIDI 乐器映射
(define instrument-map
  (hash
   'acoustic-grand-piano 0
   'bright-acoustic-piano 1
   'electric-grand-piano 2
   'honky-tonk-piano 3
   'electric-piano-1 4
   'electric-piano-2 5
   'harpsichord 6
   'clavinet 7
   'celesta 8
   'glockenspiel 9
   'music-box 10
   'vibraphone 11
   'marimba 12
   'xylophone 13
   'tubular-bells 14
   'dulcimer 15
   'drawbar-organ 16
   'percussive-organ 17
   'rock-organ 18
   'church-organ 19
   'reed-organ 20
   'accordion 21
   'harmonica 22
   'tango-accordion 23
   'acoustic-guitar-nylon 24
   'acoustic-guitar-steel 25
   'electric-guitar-jazz 26
   'electric-guitar-clean 27
   'electric-guitar-muted 28
   'overdriven-guitar 29
   'distortion-guitar 30
   'guitar-harmonics 31
   'acoustic-bass 32
   'electric-bass-finger 33
   'electric-bass-pick 34
   'fretless-bass 35
   'slap-bass-1 36
   'slap-bass-2 37
   'synth-bass-1 38
   'synth-bass-2 39
   'violin 40
   'viola 41
   'cello 42
   'contrabass 43
   'tremolo-strings 44
   'pizzicato-strings 45
   'orchestral-harp 46
   'timpani 47
   'string-ensemble-1 48
   'string-ensemble-2 49
   'synth-strings-1 50
   'synth-strings-2 51
   'choir-aahs 52
   'voice-oohs 53
   'synth-voice 54
   'orchestra-hit 55
   'trumpet 56
   'trombone 57
   'tuba 58
   'muted-trumpet 59
   'french-horn 60
   'brass-section 61
   'synth-brass-1 62
   'synth-brass-2 63
   'soprano-sax 64
   'alto-sax 65
   'tenor-sax 66
   'baritone-sax 67
   'oboe 68
   'english-horn 69
   'bassoon 70
   'clarinet 71
   'piccolo 72
   'flute 73
   'recorder 74
   'pan-flute 75
   'blown-bottle 76
   'shakuhachi 77
   'whistle 78
   'ocarina 79
   'lead-1-square 80
   'lead-2-sawtooth 81
   'lead-3-calliope 82
   'lead-4-chiff 83
   'lead-5-charang 84
   'lead-6-voice 85
   'lead-7-fifths 86
   'lead-8-bass-lead 87
   'pad-1-new-age 88
   'pad-2-warm 89
   'pad-3-polysynth 90
   'pad-4-choir 91
   'pad-5-bowed 92
   'pad-6-metallic 93
   'pad-7-halo 94
   'pad-8-sweep 95
   'fx-1-rain 96
   'fx-2-soundtrack 97
   'fx-3-crystal 98
   'fx-4-atmosphere 99
   'fx-5-brightness 100
   'fx-6-goblins 101
   'fx-7-echoes 102
   'fx-8-sci-fi 103
   'sitar 104
   'banjo 105
   'shamisen 106
   'koto 107
   'kalimba 108
   'bagpipe 109
   'fiddle 110
   'shanai 111
   'tinkle-bell 112
   'agogo 113
   'steel-drums 114
   'woodblock 115
   'taiko-drum 116
   'melodic-tom 117
   'synth-drum 118
   'reverse-cymbal 119
   'guitar-fret-noise 120
   'breath-noise 121
   'seashore 122
   'bird-tweet 123
   'telephone-ring 124
   'helicopter 125
   'applause 126
   'gunshot 127))

;; 音符名称到 MIDI 编号的映射
(define note-names
  (hash
   'C 0 'C# 1 'Db 1
   'D 2 'D# 3 'Eb 3
   'E 4
   'F 5 'F# 6 'Gb 6
   'G 7 'G# 8 'Ab 8
   'A 9 'A# 10 'Bb 10
   'B 11))

;; 工具函数: 音符名称+八度转 MIDI 编号
(define (note->midi name octave)
  (+ (* (+ octave 1) 12)
     (hash-ref note-names name)))

;; 工具函数: MIDI 编号转音符名称
(define (midi->note midi-num)
  (let* ([names '(C C# D D# E F F# G G# A A# B)]
         [octave (- (quotient midi-num 12) 1)]
         [note-idx (remainder midi-num 12)])
    (list (list-ref names note-idx) octave)))

;; 变长数量编码 (VLQ)
(define (encode-vlq value)
  (define (groups v)
    (if (< v #x80)
        (list v)
        (append (groups (arithmetic-shift v -7))
                (list (bitwise-and v #x7f)))))
  (define gs (groups value))
  (bytes->immutable-bytes
   (list->bytes
    (append (for/list ([g (drop-right gs 1)])
              (bitwise-ior #x80 g))
            (list (last gs))))))

;; 变长数量解码
(define (decode-vlq port)
  (let loop ([value 0]
             [byte (read-byte port)])
    (if (< byte #x80)
        (+ (arithmetic-shift value 7) byte)
        (loop (+ (arithmetic-shift value 7) (bitwise-and byte #x7f))
              (read-byte port)))))

;; 写入大端序整数
(define (write-u16be port value)
  (write-byte (bitwise-and (arithmetic-shift value -8) #xFF) port)
  (write-byte (bitwise-and value #xFF) port))

(define (write-u32be port value)
  (write-byte (bitwise-and (arithmetic-shift value -24) #xFF) port)
  (write-byte (bitwise-and (arithmetic-shift value -16) #xFF) port)
  (write-byte (bitwise-and (arithmetic-shift value -8) #xFF) port)
  (write-byte (bitwise-and value #xFF) port))

;; 读取大端序整数
(define (read-u16be port)
  (+ (arithmetic-shift (read-byte port) 8)
     (read-byte port)))

(define (read-u32be port)
  (+ (arithmetic-shift (read-byte port) 24)
     (arithmetic-shift (read-byte port) 16)
     (arithmetic-shift (read-byte port) 8)
     (read-byte port)))

(provide
 ;; 消息类型常量
 NOTE-OFF NOTE-ON POLY-PRESSURE CONTROL-CHANGE
 PROGRAM-CHANGE CHANNEL-PRESSURE PITCH-BEND
 META-STATUS SYSEX-START SYSEX-END
 ;; 映射表
 drum-map instrument-map note-names
 ;; 工具函数
 note->midi midi->note
 encode-vlq decode-vlq
 write-u16be write-u32be
 read-u16be read-u32be)
