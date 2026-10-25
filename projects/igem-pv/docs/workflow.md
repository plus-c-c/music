# 工作流与脚本

所有脚本都在项目根目录 `projects/igem-pv/`。

## 1. 远程 Aria-AMT 转录

```bash
cd /home/cjc/Repository/music
projects/igem-pv/transcribe.sh
```

- 上传本地 MP3 到 `iGEMServer`。
- 每个文件使用独立后台 worker。
- 默认 5 个 worker、每个 3 个 CPU 线程。
- 下载 MIDI 到 `/home/cjc/Downloads/iGEM 配乐/aria-amt-midi/`。
- 已有 MIDI 默认跳过；需要覆盖时加 `--force`。

相关文件：

```text
transcribe.sh
remote_run_cpu.py
aria-amt-cpu.patch
```

## 2. 音量平衡与循环候选

```bash
cd /home/cjc/Repository/music
python3 projects/igem-pv/igem_music.py normalize
python3 projects/igem-pv/igem_music.py detect
python3 projects/igem-pv/igem_music.py render
python3 projects/igem-pv/igem_music.py play --mode loops
```

- `normalize`：MP3 和 MIDI 渲染音频峰值统一。
- `detect`：检测循环候选。
- `render`：生成单曲循环预览。
- `play`：播放循环、原始 MP3、MIDI 或交替版本。

## 3. 15 分钟背景

```bash
python3 projects/igem-pv/presentation_background.py
```

配置：

```text
projects/igem-pv/presets/presentation-background-2026.json
```

- 每段包含起始段、完整循环、结束段。
- 段间 1.75 秒空隙。
- 背景基础版目标：`-35.2 LUFS`。
- 输出：
  - `igem-presentation-background-15min.wav/mp3`
  - `presentation-background-cues.json`

## 4. 人声循环与 +10 dB 对照

```bash
python3 projects/igem-pv/presentation_voice_loop.py --background-gain-db 10
```

或一键重建：

```bash
projects/igem-pv/render-presentation.sh
```

输出：

```text
presentation-voice-loop-15min.mp3
presentation-background-plus10db.mp3
presentation-voice-loop-with-background-15min.mp3
presentation-voice-loop-levels.json
```

## 5. 硬件段同时试听

```bash
python3 projects/igem-pv/presentation_isolate.py --play
```

生成：

```text
presentation-hardware-background-only.mp3
presentation-voice-reference.mp3
presentation-hardware-background-with-voice.mp3
```

`--play` 播放**硬件背景 + 人声同时混合版**，不是先后播放。

## 6. 固定响度表

```bash
python3 projects/igem-pv/presentation_loudness.py
```

固定列：

```text
段落 | 起始段 | 循环 | 结束段 | 音乐长度 | 段后空隙 |
人声 LUFS | 背景 LUFS | 差值
```

输出 Markdown 和 JSON：

```text
presentation-loudness-report.md
presentation-loudness-report.json
```

## 7. 全曲播放

生成后播放完整 15 分钟：

```bash
projects/igem-pv/play-full-mix.sh
```

该脚本播放：

```text
presentation-voice-loop-with-background-15min.mp3
```
