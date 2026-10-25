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

注意：`show` 返回的是普通 Racket 字符串；REPL / `C-j` 会按合法 Racket
字符串形式显示，即外层带双引号，内部换行用 `\n` 转义。例如：

```text
(show 'melody)
"Melody\n  0.0  +5/4  rest 5/16\n  1.25  +4  rest 1\n  ..."
```

`C-j` 对普通表达式会把结果写在表达式下一行，并把光标留在结果行之后，
行为与原生 `*scratch*` 一致；如果表达式没有返回值（void），则留下一个空行，
并把光标移动到该空行。对 `require`、`define` 等模块级形式会提交到 REPL，
同样留下空行，因此：

```racket
(require racket-music/kaitou)
```

可以直接在 scratch 中对这一行按 `C-j`。

## 移除链接

```bash
raco link --user --remove racket-music
```

移动仓库后需要重新执行 `raco link`。
