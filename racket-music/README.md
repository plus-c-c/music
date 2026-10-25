# `#lang racket-music`

`racket-music` 是当前 music 仓库提供的本地 Racket 语言。它把完整 Racket
与仓库的音乐 DSL / 符号环境合并到一个 `#lang` 中。

## 安装到本地 Racket

```bash
raco link --user /home/cjc/Repository/music/racket-music
```

验证：

```bash
racket -e '(require racket-music/main) (displayln "ok")'
```

## 使用

```racket
#lang racket-music

(show 'melody)
(play 'aria-intro)

(define-voice scratch-voice
  (part "Scratch"
        (note 1/4 60)
        (chord 1/4 62 'major)))
```

该语言自动提供：

- 完整 Racket；
- `midi-lisp.rkt` 的符号环境、展开、显示、播放 API；
- `demo/kaitou-lisp.rkt` 中的当前前奏绑定：
  - `piano-high`
  - `piano-low`
  - `melody`
  - `pedal-voice`
  - `aria-intro`
  - 对应符号 `'piano-high` / `'melody` / `'aria-intro` 等。

## 名称冲突

音乐 DSL 的以下名字覆盖同名 Racket/集合库绑定：

```text
eighth
rest
expand
```

因此 `#lang racket-music` 中：

- `rest` 是音乐 DSL 的休止构造器；
- `expand` 是音乐展开函数；
- `eighth` 是音乐时值常量。

需要 Racket 原绑定时可显式加前缀，例如：

```racket
(require (prefix-in list: racket/list))
(list:rest '(1 2 3))
```

## 在 Emacs 通用 scratch 中使用

先安装 collection link，然后：

```text
C-u M-x racket-scratch
```

输入：

```text
racket-music
```

即可得到带有音乐环境的交互式 Racket 会话。

## 移除链接

```bash
raco link --user --remove racket-music
```

移动仓库后需要重新执行 `raco link`。
