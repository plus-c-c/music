#lang racket/base

;; =============================================================================
;; sf2-loader.rkt — SF2/SFZ 音色库加载器
;; 通过 FluidSynth C 库 FFI 加载和播放 SoundFont 音色
;; =============================================================================

(require racket/class
         racket/list
         racket/string
         racket/path
         racket/file
         ffi/unsafe)

(provide sf2%
         sfz%
         make-sf2
         make-sf2-from-registry
         sf2-list-available
         preset-info
         preset-info-bank
         preset-info-num
         preset-info-name
         *sf2-search-paths*)

;; ---- 搜索路径 ----

(define *sf2-search-paths*
  (let ([script-dir (path-only (resolved-module-path-name
                                (variable-reference->resolved-module-path
                                 (#%variable-reference))))])
    (list (build-path script-dir "sf2")
          (build-path script-dir "samples")
          (build-path (find-system-path 'home-dir) ".local" "share" "soundfonts")
          "/usr/share/soundfonts"
          "/usr/share/sounds/sf2")))

;; ---- FluidSynth FFI ----

;; 尝试加载 FluidSynth 库
(define fluidsynth-lib
  (with-handlers ([exn:fail? (λ (e) #f)])
    (ffi-lib "libfluidsynth" '("3" "2" "1" ""))))

(define-syntax-rule (define-fluidsynth name type)
  (define name (get-ffi-obj 'name fluidsynth-lib type
                            (lambda () (lambda args (error 'fluidsynth "~a 不可用" 'name))))))

(define-fluidsynth new_fluid_settings (_fun -> _pointer))
(define-fluidsynth delete_fluid_settings (_fun _pointer -> _void))
(define-fluidsynth new_fluid_synth (_fun _pointer -> _pointer))
(define-fluidsynth delete_fluid_synth (_fun _pointer -> _void))
(define-fluidsynth fluid_synth_sfload (_fun _pointer _string _int -> _int))
(define-fluidsynth fluid_synth_sfunload (_fun _pointer _int _int -> _int))
(define-fluidsynth fluid_synth_program_select (_fun _pointer _int _int _int _int -> _int))
(define-fluidsynth fluid_synth_noteon (_fun _pointer _int _int _int -> _int))
(define-fluidsynth fluid_synth_noteoff (_fun _pointer _int _int -> _int))
(define-fluidsynth fluid_synth_cc (_fun _pointer _int _int _int -> _int))
(define-fluidsynth fluid_synth_pitch_bend (_fun _pointer _int _int -> _int))
(define-fluidsynth fluid_synth_set_gain (_fun _pointer _float -> _int))
(define-fluidsynth fluid_synth_get_sfont_count (_fun _pointer -> _int))
(define-fluidsynth fluid_synth_get_sfont_by_id (_fun _pointer _int -> _pointer))
(define-fluidsynth new_fluid_audio_driver (_fun _pointer _pointer -> _pointer))
(define-fluidsynth delete_fluid_audio_driver (_fun _pointer -> _void))
(define-fluidsynth fluid_settings_setstr (_fun _pointer _string _string -> _int))
(define-fluidsynth fluid_settings_setint (_fun _pointer _string _int -> _int))
(define-fluidsynth fluid_synth_process (_fun _pointer _int _int _pointer _int _pointer -> _int))
(define-fluidsynth fluid_synth_set_reverb_on (_fun _pointer _int -> _int))
(define-fluidsynth fluid_synth_set_chorus_on (_fun _pointer _int -> _int))

;; sfont 查询
(define-fluidsynth fluid_sfont_get_id (_fun _pointer -> _int))
(define-fluidsynth fluid_sfont_get_name (_fun _pointer -> _string))
(define-fluidsynth fluid_sfont_get_preset (_fun _pointer _int _int -> _pointer))
(define-fluidsynth fluid_sfont_iteration_start (_fun _pointer -> _void))
(define-fluidsynth fluid_sfont_iteration_next (_fun _pointer -> _pointer))
(define-fluidsynth fluid_preset_get_name (_fun _pointer -> _string))
(define-fluidsynth fluid_preset_get_banknum (_fun _pointer -> _int))
(define-fluidsynth fluid_preset_get_num (_fun _pointer -> _int))

;; ---- SF2 预设信息结构 ----

(struct preset-info (bank num name) #:transparent)

;; ---- SF2% 类 ----

(define sf2%
  (class* object% ()
    (super-new)

    (init-field [path #f]         ; SF2 文件路径
                [synth #f]        ; FluidSynth 合成器指针
                [settings #f])    ; FluidSynth 设置指针

    (field [sfont-id #f]         ; SoundFont ID
           [presets '()]          ; 可用预设列表
           [loaded? #f])

    ;; 加载 SF2 文件
    (define/public (load! [audio-driver? #f])
      (unless path
        (error 'sf2 "未指定 SF2 文件路径"))
      (unless (file-exists? path)
        (error 'sf2 "文件不存在: ~a" path))

      ;; 初始化 FluidSynth
      (set! settings (new_fluid_settings))
      (fluid_settings_setstr settings "audio.driver" "file")
      (set! synth (new_fluid_synth settings))

      ;; 加载 SoundFont
      (set! sfont-id (fluid_synth_sfload synth (if (path? path) (path->string path) path) 1))
      (when (< sfont-id 0)
        (error 'sf2 "加载失败: ~a" path))

      ;; 枚举预设
      (set! presets (enum-presets))
      (set! loaded? #t)
      (eprintf "SF2 加载成功: ~a (~a 个预设)\n"
               (file-name-from-path path) (length presets))
      this)

    ;; 卸载
    (define/public (unload!)
      (when synth (delete_fluid_synth synth) (set! synth #f))
      (when settings (delete_fluid_settings settings) (set! settings #f))
      (set! loaded? #f)
      this)

    ;; 选择程序 (bank, preset)
    (define/public (select-program! channel bank-num preset-num)
      (ensure-loaded!)
      (fluid_synth_program_select synth channel sfont-id bank-num preset-num))

    ;; 按名称选择预设
    (define/public (select-by-name! channel name)
      (ensure-loaded!)
      (let ([p (findf (λ (p) (string=? (preset-info-name p) name)) presets)])
        (unless p
          (error 'sf2 "预设未找到: ~a" name))
        (select-program! channel (preset-info-bank p) (preset-info-num p))))

    ;; 播放音符
    (define/public (noteon channel note velocity)
      (ensure-loaded!)
      (fluid_synth_noteon synth channel note velocity))

    ;; 停止音符
    (define/public (noteoff channel note)
      (ensure-loaded!)
      (fluid_synth_noteoff synth channel note))

    ;; 控制器变更
    (define/public (cc channel controller value)
      (ensure-loaded!)
      (fluid_synth_cc synth channel controller value))

    ;; 弯音
    (define/public (pitch-bend channel value)
      (ensure-loaded!)
      (fluid_synth_pitch_bend synth channel value))

    ;; 设置增益
    (define/public (set-gain! gain)
      (ensure-loaded!)
      (fluid_synth_set_gain synth (exact->inexact gain)))

    ;; 开关混响
    (define/public (set-reverb! on?)
      (ensure-loaded!)
      (fluid_synth_set_reverb_on synth (if on? 1 0)))

    ;; 开关合唱
    (define/public (set-chorus! on?)
      (ensure-loaded!)
      (fluid_synth_set_chorus_on synth (if on? 1 0)))

    ;; 获取预设列表
    (define/public (get-presets) presets)

    ;; 获取预设数量
    (define/public (preset-count) (length presets))

    ;; 渲染音频到 buffer (非实时)
    (define/public (render-sample-count len channels)
      (ensure-loaded!)
      (let ([left (malloc _float len 'raw)]
            [right (malloc _float len 'raw)])
        (fluid_synth_process synth len 2 left len right)
        (values left right)))

    ;; ---- 内部方法 ----

    (define/private (ensure-loaded!)
      (unless loaded?
        (error 'sf2 "SF2 未加载，请先调用 load!")))

    (define/private (enum-presets)
      (let* ([result '()]
             [sfont-ptr (fluid_synth_get_sfont_by_id synth sfont-id)])
        (fluid_sfont_iteration_start sfont-ptr)
        (let loop ()
          (let ([p (fluid_sfont_iteration_next sfont-ptr)])
            (when p
              (set! result
                    (cons (preset-info
                           (fluid_preset_get_banknum p)
                           (fluid_preset_get_num p)
                           (fluid_preset_get_name p))
                          result))
              (loop))))
        (sort result < #:key (λ (p) (+ (* 128 (preset-info-bank p)) (preset-info-num p))))))

    ;; 自动清理
    (define/public (finalize)
      (unload!))
    ))

;; ---- 便捷构造 ----

(define (make-sf2 path [audio-driver? #f])
  (let ([sf (new sf2% [path path])])
    (send sf load! audio-driver?)
    sf))

;; ---- SFZ 加载器 (简单解析 .sfz 文件) ----

(define sfz%
  (class* object% ()
    (super-new)

    (init-field [path #f])       ; .sfz 文件路径

    (field [regions '()]          ; 区域列表: (list (hash 'sample ... 'lokey ... 'hikey ...))
           [groups '()]           ; 组列表
           [loaded? #f])

    ;; 加载 SFZ 文件
    (define/public (load!)
      (unless path
        (error 'sfz "未指定 SFZ 文件路径"))
      (unless (file-exists? path)
        (error 'sfz "文件不存在: ~a" path))

      (define base-dir (path-only (if (path? path) path (string->path path))))
      (define lines (file->lines path))
      (define current-group (make-hash))
      (define current-region (make-hash))
      (define in-region? #f)

      (for ([line lines])
        (define trimmed (string-trim line))
        (cond
          ;; 空行或注释
          [(or (string=? trimmed "") (string-prefix? trimmed "//")) (void)]
          ;; <group>
          [(string-prefix? trimmed "<group>")
           (when in-region?
             (when (> (hash-count current-region) 0)
               (set! regions (cons current-region regions)))
             (set! current-region (make-hash))
             (set! in-region? #f))
           (set! current-group (parse-opcodes (substring trimmed 7)))]
          ;; <region>
          [(string-prefix? trimmed "<region>")
           (when in-region?
             (when (> (hash-count current-region) 0)
               (set! regions (cons current-region regions))))
           (set! current-region (hash-copy current-group))
           (set! in-region? #t)
           (for ([k-v (parse-opcodes (substring trimmed 8))])
             (hash-set! current-region (car k-v) (cdr k-v)))]
          ;; opcodes (无标签)
          [in-region?
           (for ([k-v (parse-opcodes trimmed)])
             (hash-set! current-region (car k-v) (cdr k-v)))]
          [else
           (for ([k-v (parse-opcodes trimmed)])
             (hash-set! current-group (car k-v) (cdr k-v)))]))

      ;; 保存最后一个 region
      (when (and in-region? (> (hash-count current-region) 0))
        (set! regions (cons current-region regions)))

      (set! loaded? #t)
      (eprintf "SFZ 加载成功: ~a (~a 个区域)\n"
               (file-name-from-path path) (length regions))
      this)

    ;; 获取所有采样路径
    (define/public (get-samples)
      (ensure-loaded!)
      (for/list ([r regions] #:when (hash-has-key? r 'sample))
        (let ([s (hash-ref r 'sample)])
          (if (absolute-path? s)
              s
              (build-path (path-only (if (path? path) path (string->path path))) s)))))

    ;; 获取区域数量
    (define/public (region-count) (length regions))

    ;; 按 key 查找区域
    (define/public (find-region note)
      (ensure-loaded!)
      (findf (λ (r)
               (let ([lo (hash-ref r 'lokey 0)]
                     [hi (hash-ref r 'hikey 127)])
                 (<= lo note hi)))
             regions))

    ;; ---- 内部 ----

    (define/private (ensure-loaded!)
      (unless loaded?
        (error 'sfz "SFZ 未加载，请先调用 load!")))

    (define/private (parse-opcodes str)
      (for/list ([token (string-split str)]
                 #:when (string-contains? token "="))
        (let* ([parts (string-split token "=")]
               [key (string->symbol (car parts))]
               [val (cadr parts)])
          (cons key (parse-value val)))))

    (define/private (parse-value str)
      (cond
        [(string->number str) => (λ (x) x)]
        [else (string->symbol str)]))
    ))

;; ---- 便捷构造 ----

(define (make-sfz path)
  (let ([sf (new sfz% [path path])])
    (send sf load!)
    sf))

;; ---- 音色库注册表 ----

(define *sf2-registry* (make-hash))  ; name -> path

(define (sf2-list-available)
  (define result '())
  (for ([dir *sf2-search-paths*]
        #:when (directory-exists? dir))
    (for ([f (directory-list dir #:build? #t)]
          #:when (or (path-has-extension? f #".sf2")
                     (path-has-extension? f #".sf3")))
      (set! result (cons (cons (file-name-from-path f) f) result))))
  ;; 也搜索 SFZ
  (for ([dir *sf2-search-paths*]
        #:when (directory-exists? dir))
    (for ([f (directory-list dir #:build? #t)]
          #:when (path-has-extension? f #".sfz"))
      (set! result (cons (cons (file-name-from-path f) f) result))))
  (sort result string<? #:key (λ (p) (path->string (car p)))))

(define (make-sf2-from-registry name)
  (unless (hash-has-key? *sf2-registry* name)
    ;; 尝试在搜索路径中查找
    (for ([dir *sf2-search-paths*]
          #:when (directory-exists? dir))
      (for ([f (directory-list dir #:build? #t)]
            #:when (path-has-extension? f #".sf2"))
        (when (string=? (path->string (file-name-from-path f)) name)
          (hash-set! *sf2-registry* name f)))))
  (unless (hash-has-key? *sf2-registry* name)
    (error 'sf2-loader "未找到音色库: ~a" name))
  (make-sf2 (hash-ref *sf2-registry* name)))
