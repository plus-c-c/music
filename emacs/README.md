# Emacs 音乐编程环境

本目录把仓库接入 Emacs 的交互式 Lisp 工作流。第一阶段先实现一个
DrRacket 风格的 **定义区 + 交互区**，并保留 Emacs `*scratch*` 的直接求值体验。

## 1. 当前 Emacs 环境审计

本机环境：

```text
GNU Emacs 31.1
racket-mode 20260926.1912
gptel 20261013.2205（elpa 中同时存在 20260911.202）
OpenCode Emacs 集成（site-lisp/opencode）
tree-sitter-racket
lsp-bridge
```

已确认的情况：

- `racket-mode` 已安装并配置：
  - `racket-run` / `racket-run-and-switch-to-repl`
  - `racket-repl-mode`
  - `racket-send-region`
  - `racket-eval-last-sexp`
  - `racket-xp-mode`、调试、profile、stepper
- 当前配置 `init.d/dev/lang/config-racket.el` 把 `C-c C-r` 绑到 `racket-run`；
  在音乐 scratch 中，本包把它覆盖为“发送 region 到 REPL”。
- `gptel` 已配置 Ollama / DeepSeek，支持 region rewrite、context、tools；
  但 `init.d/edit/init-edit.el` 和 `init.d/edit/config-gptel.el` 有重复配置，
  后续应合并。
- OpenCode 集成支持 session、LSP、worktree、region、agent，适合仓库级改动。
- `ellama` 只有历史 session，当前 `(locate-library "ellama")` 为 nil；
  第一阶段不依赖它。
- `racket-langserver` 当前不在 PATH 中；lsp-bridge 的 Racket LSP 需要另行安装。
- `config-racket.el` 里 DAP 配置调用了当前 racket-mode 中不存在的
  `racket--repl-and-process`；Racket 调试应使用 `racket-debug-mode`。

## 2. 外部资料结论

- `racket-mode` 明确以 DrRacket 的概念为参考，提供 Run、REPL、发送 region /
  definition、check-syntax、debug、stepper 等：
  <https://racket-mode.com/>
- Racket 官方 Emacs 文档也把 racket-mode 描述为 DrRacket-style REPL 和
  buffer execution：
  <https://docs.racket-lang.org/guide/Emacs.html>
- Emacs 原生 `*scratch*` 使用 `lisp-interaction-mode`，`C-j` 执行前一个
  sexp 并把值写回 buffer：
  <https://www.gnu.org/software/emacs/manual/html_node/emacs/Lisp-Interaction.html>
- `persistent-scratch` / `scratch-plus` 证明 scratch 的“持久化、可恢复”是
  常见需求：
  <https://github.com/Fanael/persistent-scratch>
- gptel 的 `gptel-rewrite` 支持 region 预览、diff、接受/拒绝；gptel context
  可以加入当前 buffer、文件、目录：
  <https://github.com/karthink/gptel>
  <https://gptel.org/manual.html>
- Org Babel 的 Racket 支持（ob-racket / ob-scheme）对 session 支持有限，
  不适合作为第一阶段的实时音乐 REPL：
  <https://github.com/DEADB17/ob-racket>

结论：**以 racket-mode 为执行内核，以 midi-lisp 为音乐语义层，以
gptel/OpenCode 为 AI 编辑层**，比重新实现一个 Racket 解释器更稳。

## 3. 第一阶段：通用 scratch + 音乐扩展

### 3.1 Emacs 配置仓库中的通用能力

在 Emacs 配置仓库：

```text
~/.emacs.d/init.d/dev/lang/config-racket.el
```

加入通用的 `racket-scratch`：

```text
M-x racket-scratch          新建一个通用 Racket scratch + 独立 REPL
C-u M-x racket-scratch      新建时询问任意 #lang
C-c C-n                     在 racket-mode 中创建通用 scratch（绑定于 config-racket）
C-c C-c                     运行 scratch
C-j                         执行前一个 sexp 并把结果写回 buffer
C-c C-r                     发送 region
C-c C-z                     切换到该 scratch 的 REPL
C-c C-i f                   导入一个 .rkt 文件
C-c C-i m                   导入一个 Racket module
```

生成的文件位于：

```text
~/.emacs.d/racket-scratch/scratch-*.rkt
```

每个 scratch 有独立的 `racket-repl-buffer-name`，所以可以同时开多个
互不干扰的交互式 Racket session。默认采用 DrRacket 的“编辑后 Run”流程：

```text
M-x racket-scratch
C-c C-c
```

如需打开即启动 REPL，可设置：

```elisp
(setq racket-scratch-auto-start t)
```

通用配置本身不包含任何音乐仓库路径；音乐专用命令、voice 绑定和播放逻辑
全部保留在 music 仓库中。使用当前仓库时，在 scratch 中导入：

```racket
(require (file "/home/cjc/Repository/music/midi-lisp.rkt"))
(require (file "/home/cjc/Repository/music/demo/kaitou-lisp.rkt"))
```

或交互式：

```text
C-c C-i f  ~/Repository/music/demo/kaitou-lisp.rkt
C-c C-c
```

然后直接使用：

```racket
(show 'melody)
(play 'aria-intro)
```

### 3.1.1 直接使用 `#lang racket-music`

仓库提供本地 Racket 语言：

```text
racket-music/
```

安装 collection link：

```bash
raco link --user /home/cjc/Repository/music/racket-music
```

之后可以直接写通用音乐代码：

```racket
#lang racket-music

(define-voice scratch-voice
  (part "Scratch"
        (note 1/4 60)
        (chord 1/4 62 'major)))
```

当前 Aria 前奏 demo 不包含在语言中，需要单独导入：

```racket
#lang racket-music
(require racket-music/kaitou)

(show 'melody)
(play 'aria-intro)
```

在通用 Emacs scratch 中：

```text
C-u M-x racket-scratch
```

输入 `racket-music`，得到通用 DSL 会话；需要 demo 时：

```text
C-c C-i m  racket-music/kaitou
C-c C-c
```

注意：该语言中 `eighth`、`rest`、`expand` 使用音乐 DSL 定义，
会覆盖 Racket 的同名绑定；需要 Racket 原绑定时可加前缀导入。
详见 `racket-music/README.md`。

### 3.2 音乐仓库中的专用扩展

音乐仓库额外提供可选的音乐专用封装：

```text
emacs/music-scratch.el
```

通用场景优先使用 `racket-scratch`；`music-scratch.el` 适合需要
`C-c C-e` / `C-c C-x` / `C-c C-p` 等音乐语义快捷键的场景。

它提供一个持久化的 `*Music Scratch*`：

- 文件后端：`~/.emacs.d/music-scratch.rkt`
- 主模式：`racket-mode`
- 交互区：`*Music REPL*`
- 启动时自动 `require`：
  - `midi-lisp.rkt`
  - `demo/kaitou-lisp.rkt`

### 安装

```elisp
(add-to-list 'load-path "~/Repository/music/emacs")
(require 'music-scratch)
(global-set-key (kbd "C-c m") #'music-scratch)
```

或：

```elisp
(use-package music-scratch
  :load-path "~/Repository/music/emacs"
  :bind ("C-c m" . music-scratch))
```

### 使用

```text
M-x music-scratch

C-c C-c   运行 scratch 文件并启动/刷新 Racket REPL
C-j       执行前一个表达式，把结果插入 scratch buffer
C-c C-r   发送 region 到 REPL
C-c C-z   切换到 REPL
C-c C-e   显示光标处音乐符号的展开结构
C-c C-x   在 REPL 中执行 (expand 'symbol)
C-c C-p   播放光标处音乐符号
```

`C-j` 的行为接近原生 `*scratch*`：

```racket
(+ 1 2) => 3
```

音乐符号：

```racket
(show 'melody)
(play 'aria-intro)
(play (expand 'aria-intro))
```

### 设计要点

- **定义区**和 **交互区**分离：`*Music Scratch*` 负责编辑 RKT 源码，
  `*Music REPL*` 负责即时结果与音频副作用。
- **scratch 持久化**：文件后端让实验内容在 Emacs 重启后仍在；
  同时保留 `*Music Scratch*` 这个类似原生 scratch 的 buffer 名称。
- **直接求值**：`racket-eval-last-sexp` 返回结果通过 buffer-local
  `racket-show-functions` 插回 scratch，形成 `expr => value` 记录。
- **符号环境**：`midi-lisp` 的 `interpret` / `expand` / `show` / `play`
  同时接受符号、展开前 voice/score、展开后 expanded-voice。
- **不阻塞 Emacs**：播放和长计算在 Racket 后端执行；Emacs 只负责编辑和展示。

## 4. 与 AI 辅助修改的结合

推荐分三层，不要把所有能力塞进一个命令：

### 4.1 gptel：局部代码修改

适合：

- 修改一个 voice、一个 loop 段、一个踏板区间；
- 根据当前 `show`/`expand` 结果生成变体；
- 把自然语言编曲意图转成 `note` / `chord` / `pedal` 形式。

交互方式：

1. 选中 RKT 或 scratch 中的 sexp；
2. `gptel-rewrite`；
3. 在 overlay 中预览 diff；
4. 接受后立刻 `C-c C-c` 运行；
5. 用 `(show ...)` / `(play ...)` 人工试听；
6. 用 `raco test` 和渲染结果做回归验证。

gptel context 应包含：

- 当前 region；
- `midi-lisp.rkt` 的 API 摘要；
- 相关 voice 的 `expand` 结果；
- 项目约束（时值分母、tempo、loop 三段规则、踏板规则）。

### 4.2 OpenCode：仓库级重构

适合：

- 同时修改 `midi-*.rkt`、demo 和测试；
- 跨文件的数据流重构；
- 运行测试、修复错误、生成迁移说明。

OpenCode 已有的 LSP / worktree / session 能力适合第二层；
但音频播放和“听感判断”仍应由人完成，AI 只提交候选。

### 4.3 MCP / 工具层

后续可提供一个只读/受控的 MCP server：

```text
music_eval(expr)       -> 受限 Racket 求值结果
music_expand(symbol)   -> 展开结构
music_show(symbol)     -> 可读谱面
music_play(symbol)     -> 触发试听
music_render(symbol)   -> MIDI/WAV 文件路径
music_test()           -> raco test 摘要
```

AI 生成的内容只写入 **候选 diff**；由人或明确的接受命令落盘。
禁止 AI 直接修改已播放状态、后台音频进程或未提交的多个文件。

## 5. 后续路线

1. **当前阶段**：完成 scratch + REPL + C-j + show/play 命令。
2. 增加 music-specific expand 可视化（小节线、tie、CC64）。
3. 接入 gptel context 函数，自动附带当前 sexp 的展开结果。
4. 增加 MCP server 和 OpenCode 工具。
5. 增加 Org Babel 集成（先做非 session 的“生成 score”块，
   再做 racket-mode 的 session 桥接）。
6. 把 AI 修改纳入测试门禁：`raco test`、结构检查、渲染检查、听感确认。
