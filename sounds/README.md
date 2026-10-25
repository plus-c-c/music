# sounds/ — 音色库管理

## 目录结构

```
sounds/
├── sf2/                    # SoundFont 2 音色库
├── sfz/                    # SFZ 采样音色库
├── samples/                # 通用音频采样
├── ardour-presets/          # 从 Ardour 提取的 VST 预设
├── download-libraries.sh    # 下载脚本
├── extract-ardour-presets.sh # Ardour 预设提取脚本
├── sf2-loader.rkt           # Racket SF2 加载模块
├── Makefile                 # 自动化构建
└── README.md               # 本文件
```

## 快速开始

```bash
# 下载所有音色库
make all

# 仅下载 SF2
make sf2

# 仅下载 SFZ
make sfz

# 列出可用音色
make list
```

## 在 Racket 中使用

```racket
#lang racket/base

(require "sf2-loader.rkt")

;; 列出可用音色
(for ([p (sf2-list-available)])
  (printf "~a: ~a\n" (car p) (cdr p)))

;; 加载 SF2
(define synth (make-sf2 "sf2/arachno-gm.sf2"))

;; 选择预设 (GM Piano)
(send synth select-by-name! 0 "Piano 1")

;; 播放 C4 (MIDI note 60)
(send synth noteon 0 60 80)

;; 32768 samples later...
(send synth noteoff 0 60)

;; 加载 SFZ
(define violin (make-sfz "sfz/sonatina-orchestra/strings/violin.sfz"))
```

## Ardour 预设提取

```bash
# 需要先安装 ardour2fxp
pip install ardour2fxp

# 提取预设
./extract-ardour-presots.sh extract

# 列出预设
./extract-ardour-presots.sh list
```

## 依赖

- `curl` 或 `wget` (下载)
- `unzip` (解压)
- `fluidsynth` (SF2 播放，可选)
- `ardour2fxp` (Ardour 预设转换，可选)
