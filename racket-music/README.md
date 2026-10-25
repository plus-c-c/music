# `#lang racket-music`

`racket-music` 是当前 music 仓库提供的本地 Racket 语言。它把完整 Racket
与通用的音乐 DSL / 符号环境合并到一个 `#lang` 中。

注意：**demo 乐谱不属于语言本体**。`#lang racket-music` 只提供通用
DSL 与环境；当前 Aria 前奏需要单独导入 `racket-music/kaitou`。

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

;; 通用语言能力：完整 Racket + music DSL / 符号环境
(define-voice scratch-voice
  (part "Scratch"
        (note 1/4 60)
        (chord 1/4 62 'major)))

(show 'scratch-voice)
```

导入当前 Aria 前奏 demo：

```racket
#lang racket-music
(require racket-music/kaitou)

(show 'melody)
(play 'aria-intro)
```

`#lang racket-music` 自动提供：

- 完整 Racket；
- `midi-lisp.rkt` 的符号环境、展开、显示、播放 API。

`racket-music/kaitou` 单独提供：

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

先安装 collection link，然后创建通用 scratch：

```text
C-u M-x racket-scratch
```

推荐流程：

1. 安装 collection：

   ```bash
   raco link --user /home/cjc/Repository/music/racket-music
   ```

2. 新建指定 `#lang` 的 scratch：

   ```text
   C-u M-x racket-scratch
   ```

   输入：

   ```text
   racket-music
   ```

3. 如果需要当前 Aria 前奏，单独导入 demo：

   ```text
   C-c C-i m  racket-music/kaitou
   ```

   或在 scratch 中直接写：

   ```racket
   #lang racket-music
   (require racket-music/kaitou)
   ```

4. 运行：

   ```text
   C-c C-c
   ```

5. 在 scratch 或 REPL 中：

   ```racket
   (show 'melody)
   (play 'aria-intro)
   ```

`#lang racket-music` 本身只提供通用 DSL；demo 的 voice 绑定通过
`racket-music/kaitou` 单独导入。

## 移除链接

```bash
raco link --user --remove racket-music
```

移动仓库后需要重新执行 `raco link`。
