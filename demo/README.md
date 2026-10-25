# 开头 4 小节前奏 — Aria-AMT RKT

> 当前版本：`demo/kaitou-aria-intro.rkt`
>
> 内容：Aria-AMT 前奏 M1–M4；4 声部；75 BPM；1/16 全音符网格。

## 0. 网格化前提：先确定 tempo

网格化前先确认速度，再建立时值网格。

- 当前曲为 **75 BPM**；
- 4/4 每小节约 **3.2 秒**：`4 拍 × 60 / 75`；
- 全曲统一使用 75 BPM，不按小节单独修正相位；
- RKT 时值使用全音符分数，生成网格为 **1/16 全音符**，分母最大为 16。

## 1. 当前来源

```text
Aria-AMT 完整 27 秒 MIDI
`demo/reference/aria-amt-full-75.mid`
        │
        ├─ 按小节最大 velocity 检测主旋律起点
        │    第 5 小节最大 velocity 从 55 跳到 80
        │
        ├─ 只取第 1–4 小节（1.25–17.25 拍）
        ├─ 自动拆声部
        └─ 自动拆小节；跨小节音符/踏板用 tie 连接
```

当前前奏：

- 4 个小节，`bar-offset = 5/4` 拍；
- 56 个 note events；
- M1–M4 的 Piano Low 都在小节内 `3.25` 进入；
- M1 开头 `13/16` 是弱起 D#3 的 tie continuation；M4 低音为单音 G#1；
- M2→M3 与 M3→M4 保留前一末尾弱起，下一小节强拍重新起音；
- Piano High 在 M1–M4 使用统一节奏型；
- D#6 只属于 `Melody`；G#2 是 G#1 的谐波，不进入独立声部；
- 第一个踏板从 1.0 拍开始，经 1.0–1.25 拍前置段 tie 入 M1，使弱起 D#3 延入 M1；
- M1–M3 的踏板型为：长段 `13/16` + rest `1/8` + 短段 `1/16`；
- M4 的踏板型为：长段 `13/16` + 收尾 rest；
- 基础踏板展开为 4 个 CC64 区间：1.0–4.5、5.0–8.5、9.0–12.5、13.0–16.5。

4 个声部：

- `Piano High`：含弱起 A#4 / D#5；
- `Piano Low`：含弱起 D#3；M1–M4 低音型统一；M4 为 G#1；
- `Melody`：M4 的 D#6；
- `Pedal`：CC64。

弱起三音按音区并入 Piano High / Piano Low。A#4 / D#5 裁到 M1 小节线；D#3 从 0.5 拍起音，与 M1 的 `13/16` tie continuation 合并到 4.5 拍，并受 1.0–4.5 的第一段踏板延音。

## 2. 核心 DSL

```racket
(note length pitch [velocity] [channel])
(chord length root type ...)
(rest length)
(pedal length)                 ; CC64，长度同样是全音符分数
(part "Name" ...)
(score part ...)
(measure 1 ...)                ; 小节容器
```

时值单位是全音符分数，网格为 **1/16 全音符**：

```text
1/4  = 四分音符 = 1 拍
1/2  = 二分音符 = 2 拍
3/8  = 附点四分音符 = 1.5 拍
1/16 = 十六分音符 = 0.25 拍
```

RKT 数据不记录 `start`；`start` 只在 `expand-voice` 内部由累计时值产生。显示小节线时也只使用累计时值。

## 3. 跨小节音符与踏板

跨小节对象使用 `tie-start` / `tie-end` / `tie-continue`：

```racket
(measure 1
  (rest 1/2)
  (tie-start (note 1/2 60 90 0)))   ; 从本小节延续到下一小节

(measure 2
  (tie-end (note 1/4 60 90 0))      ; 延续自上一小节
  (rest 3/4))
```

- 跨两个小节：`tie-start` + `tie-end`；
- 跨多个小节：中间片段使用 `tie-continue`；
- `tie-start` / `tie-end` 两端必须音高、力度、通道一致；展开和播放时合并为一个 MIDI event；
- 踏板使用同一种标记：

```racket
(measure 1
  (rest 1/2)
  (tie-start (pedal 1/2)))

(measure 2
  (tie-end (pedal 1/4))
  (rest 3/4))
```

踏板片段在展开时按 tie 合并；M1–M3 每小节末尾有短抬 / 重踩，M4 收尾抬踏板。

显示时用 `~` 表示连音方向：

```text
note D5 1/16 ~
|
~ note D5 1/16
```

## 4. 循环播放

循环功能位于库 `midi-loop.rkt`，由 `midi/main.rkt` 统一导出。

### 4.1 三段结构

一个 loop 由三段组成：

```text
start 段：loop 起点之前的弱起，只播放一次
loop  段：[start, start + length) 的主体，逐遍重复
end   段：最后一遍末尾的 end-length 拍，只播放一次
```

三段都用同一个 `loop-segment` 表示：

```racket
(loop-segment events pedals start end)
```

- `start` / `end` 段分别支持独立变换：
  ```racket
  #:start-transform (lambda (segment) ...)
  #:end-transform   (lambda (segment) ...)
  ```
- 变换可以是单个函数，也可以是函数列表；列表按顺序作用于同一个 `loop-segment`，因此同一段可以叠加多个修饰；
- 同一个修饰函数可以同时传给 `start` 和 `end`；
- 仅播放一遍时，`start` 和 `end` 两段的变换仍然都会执行；
- 默认：`start` 段原样保留；`end` 段清空为 rest；
- `empty-loop-segment`：清空一段的音符和踏板；
- `loop-segment-hold-pedal`：清空音符，并用踏板覆盖整个段。

### 4.2 API

```racket
(loop-events events pedals repeats
             #:start start
             #:length length
             #:end-length [end-length 0]
             #:pickup-window [pickup-window #f]
             #:start-transform [start-transform identity]
             #:end-transform [end-transform empty-loop-segment]
             #:bridge-pedals? [bridge-pedals? #f])

(loop-score score repeats
            #:start start
            #:length length
            #:end-length [end-length 0]
            #:pickup-window [pickup-window #f]
            #:start-transform [start-transform identity]
            #:end-transform [end-transform empty-loop-segment]
            #:exclude-voices [exclude-voices '()]
            #:bridge-pedals? [bridge-pedals? #f])

(play-loop score repeats
           #:start start
           #:length length
           #:end-length [end-length 0]
           #:pickup-window [pickup-window #f]
           #:start-transform [start-transform identity]
           #:end-transform [end-transform empty-loop-segment]
           #:exclude-voices [exclude-voices '()]
           #:bridge-pedals? [bridge-pedals? #f]
           #:tempo tempo)
```

- `loop-events`：处理已经展开的 note / pedal 事件；
- `loop-score`：从多声部乐谱展开并循环，返回 `(values events pedals)`；
- `play-loop`：循环并调用播放后端；
- `merge-pedal-events`：合并首尾相接或重叠的同通道踏板区间。

### 4.3 规则

- `pickup-window`：`start` 前的弱起窗口只出现第一遍；
- `[start, start + length)` 内的材料逐遍重复；
- `end-length`：最后一遍末尾的结束段长度；
- `start-transform` / `end-transform`：分别修饰 start / end 段；可传单个函数或函数列表，列表按顺序叠加在同一段上；
- 只播放一遍时，start 段与 end 段仍分别应用各自的变换；
- `bridge-pedals?`：把每一遍最后的踏板与下一遍第一段踏板合并为连续 CC64 区间；
- `exclude-voices`：按声部名排除声部，展开前生效。

### 4.4 当前前奏循环

`demo/loop-m1m4.rkt` 是当前前奏的轻量封装：

```racket
(define (end-section-pedal-to-bar segment)
  (loop-segment-hold-pedal segment))

(loop-score piece n
            #:start 5/4
            #:length 16
            #:end-length 3/4
            #:pickup-window 3/4
            #:start-transform start-transform
            #:end-transform end-section-pedal-to-bar
            #:exclude-voices '("Melody")
            #:bridge-pedals? #t)
```

`demo/loop-m1m4.rkt` 的封装函数同样接受：

```racket
#:start-transform start-transform
#:end-transform   end-transform
```

默认 `start-transform` 原样保留，`end-transform` 使用结束段踏板延音。

- 第一遍保留 M1 弱起；
- 后续遍跳过 M1 原始弱起；
- 循环默认排除 `Melody`；
- 中间遍把 M4 末尾 `3/16`（0.75 拍）用连续踏板延入下一遍 M1；
- 最后一遍 M4 末尾 `3/16` 的音符保持 rest；
- 最后一遍 M4 的踏板延长到小节结尾，即额外延长 `3/16`（0.75 拍）；
- 单次 `M1–M4` 也调用同一个 loop 函数，只执行一遍；start / end 两段的修饰仍然分别执行。

## 5. Lisp 环境

### 5.1 导入环境

Emacs 通用流程：

```text
M-x racket-scratch
C-c C-i f  ~/Repository/music/demo/kaitou-lisp.rkt
C-c C-c
(show 'melody)
```

Racket REPL 中也可以直接：

```bash
racket -i -e '(require "demo/kaitou-lisp.rkt")'
```

`demo/kaitou-lisp.rkt` 会导入 `midi-lisp.rkt`，并注册当前前奏的声部：

```text
piano-high  / 'piano-high
piano-low   / 'piano-low
melody      / 'melody
pedal-voice / 'pedal
aria-intro  / 'aria-intro
```

### 5.2 直接使用 `#lang racket-music`

仓库提供本地 Racket 语言 `racket-music`。安装 collection link：

```bash
raco link --user /home/cjc/Repository/music/racket-music
```

`#lang racket-music` 只提供完整 Racket 与通用 `midi-lisp` 环境，
不包含 demo。当前前奏需要单独导入：

```racket
#lang racket-music
(require racket-music/kaitou)

(show 'melody)
(play 'aria-intro)
```

在通用 Emacs scratch 中：

```text
C-u M-x racket-scratch
racket-music
C-c C-i m  racket-music/kaitou
C-c C-c
```

详见 `racket-music/README.md`。

### 5.3 输入符号并展开

```racket
> piano-high
> 'piano-high
> (interpret 'piano-high)
> (expand 'piano-high)
> (show 'piano-high)
```

- `piano-high`：REPL 中的变量绑定；
- `'piano-high`：环境中的符号；
- `interpret`：把符号解析成实际的 voice / score；
- `expand`：展开成 `expanded-voice` / `expanded-voice` 列表；
- `show`：直接显示展开后的音符、和弦、休止与踏板。

示例：

```text
> (show 'melody)
Melody
  0.0  +5/4  rest 5/16
  1.25  +4  rest 1
  5.25  +4  rest 1
  9.25  +4  rest 1
  13.25  +13/4  rest 13/16
  16.5  +3/4  note D#6 3/16
```

### 5.4 展开前后的结构都可以 play

```racket
> (play 'piano-high)
> (play (expand 'piano-high))
> (play 'aria-intro)
> (play (expand 'aria-intro))
```

`play` 接受：

- 符号；
- 展开前的 `voice` / `score`；
- 展开后的 `expanded-voice` / `expanded-voice` 列表；
- 已展开的 `midi-event` 列表。

当前环境的默认播放速度为 75 BPM，可用 `#:tempo` 覆盖。需要直接取播放材料时：

```racket
> (music->events 'aria-intro)
```

返回 `(values events pedals voice-count)`。

### 5.5 定义并注册新声部

```racket
(define-voice my-voice
  (part "My Voice"
        (note 1/4 60)
        (chord 1/4 62 'major)))

(define-score my-score
  (score my-voice))
```

`define-voice` / `define-score` 会同时定义变量并写入当前环境。也可以用：

```racket
(bind-music! 'my-voice my-voice)
(interpret 'my-voice)
```

环境函数：

- `make-music-environment`
- `current-music-environment`
- `bind-music!`
- `lookup-music`
- `interpret`

## 6. 当前文件

| 文件 | 作用 |
|---|---|
| `midi/main.rkt` | 公共 API 统一入口 |
| `midi-lisp.rkt` | Lisp 风格符号环境、`interpret`、`expand`、`show`、`play` |
| `racket-music/main.rkt` | `#lang racket-music` 语言入口（只含通用 DSL） |
| `racket-music/kaitou.rkt` | 单独导入当前 Aria 前奏 demo |
| `racket-music/lang/reader.rkt` | `#lang racket-music` 的 s-exp reader |
| `racket-music/README.md` | 本地语言安装与使用说明 |
| `midi-loop.rkt` | 三段循环（start / loop / end）、段变换、踏板边界合并、循环播放 |
| `midi-dsl.rkt` | `note`、`chord`、`rest`、`pedal`、`tie`、`tie-start`、`tie-end`、`tie-continue`、`part`、`score` |
| `midi-score.rkt` | 展开、`measure`、跨小节 tie 合并、CC64 合并、播放 |
| `midi-display.rkt` | 只按时值显示，并显示 `~` 与累计小节线 |
| `midi-reader.rkt` | SMF 读取、note/pedal 解析 |
| `midi-render.rkt` | WAV 渲染与播放 |
| `midi-voices.rkt` | polyphonic 音符拆成单声部 |
| `demo/build-aria-intro.rkt` | 从 75 BPM Aria MIDI 生成前奏 RKT / MIDI / WAV |
| `demo/kaitou-aria-intro.rkt` | 当前前奏 RKT（4 小节、4 声部） |
| `demo/kaitou-lisp.rkt` | 注册 `piano-high` / `piano-low` / `melody` / `pedal-voice` / `aria-intro` 的 Lisp 环境 |
| `demo/play-aria-intro.rkt` | 单次 M1–M4，调用 loop 函数；末尾音符 rest、踏板到小节结尾 |
| `demo/play-aria-measures.rkt` | 按「弱起+M1 / M2 / M3 / M4」分段播放 |
| `demo/loop-m1m4.rkt` | M1–M4 三段循环封装（默认排除 `Melody`；结束段踏板到小节结尾） |
| `demo/show-aria-intro-structure.rkt` | 按小节展示结构、tie 与 CC64 |
| `demo/show-aria-intro-grid.rkt` | 检查 1/16 网格对齐，列出反拍 / 弱位起音 |
| `demo/reference/aria-amt-full-75.mid` | 当前 Aria-AMT 75 BPM 源 MIDI |
| `demo/reference/aria-intro.mid` | 从 RKT 展开导出的前奏 MIDI（含 CC64） |
| `demo/reference/aria-intro.wav` | 当前前奏渲染音频 |
| `test/loop-test.rkt` | 三段循环、start/end 变换、单遍修饰、同段叠加、声部排除、结束段踏板、边界合并测试 |
| `test/cross-measure-tie-test.rkt` | 跨小节音符 / 踏板 tie 测试 |
| `test/lisp-test.rkt` | 符号解析、展开、展开前后结构统一播放输入测试 |
| `test/racket-music-lang-test.rkt` | `#lang racket-music` 语言集成测试 |

## 7. 运行

```bash
cd /home/cjc/Repository/music

# 展示音乐结构
racket demo/show-aria-intro-structure.rkt
racket demo/show-aria-intro-structure.rkt --full

# 检查网格对齐
racket demo/show-aria-intro-grid.rkt

# 播放单次 M1–M4
racket demo/play-aria-intro.rkt

# 按小节播放
racket demo/play-aria-measures.rkt

# 循环 M1–M4 5 次（不含 Melody）
racket demo/loop-m1m4.rkt 5

# 只生成循环事件，不播放
racket demo/loop-m1m4.rkt --no-play 5

# 生成前奏 RKT / MIDI / WAV
racket demo/build-aria-intro.rkt

# 安装本地 Racket 语言（一次）
raco link --user /home/cjc/Repository/music/racket-music

# 启动 Lisp REPL
racket -i -e '(require "demo/kaitou-lisp.rkt")'

# 非交互播放整首前奏
racket -e '(require "demo/kaitou-lisp.rkt") (play (quote aria-intro))'

# 编译核心与 demo
raco make midi/main.rkt midi-loop.rkt midi-lisp.rkt
raco make demo/kaitou-aria-intro.rkt demo/play-aria-intro.rkt
raco make demo/kaitou-lisp.rkt

# 测试
raco test test/loop-test.rkt
raco test test/lisp-test.rkt
raco test test/racket-music-lang-test.rkt
raco test test/cross-measure-tie-test.rkt
```
