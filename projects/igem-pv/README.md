# iGEM 配乐工作流

## 文档索引

- [配乐结构与循环思路](docs/arrangement.md)
- [人声与背景响度](docs/loudness.md)
- [工作流与脚本](docs/workflow.md)
- [当前输出清单](docs/outputs.md)
- [MIDI 优化流程](docs/midi-optimization.md)
- [当前响度表快照](docs/current-loudness-report.md)

这套脚本固化当前的 iGEM 配乐处理流程：

1. 在远程 `iGEMServer` 上并行运行 Aria-AMT；
2. 测量 MP3 和 MIDI 渲染音频的峰值，生成统一音量参数；
3. 从音频节拍与和声接缝检测循环候选；
4. 按 `起始 → 循环 → 循环 → 结束` 渲染无爆音预览；
5. 按指定顺序播放预览、归一化 MIDI/MP3 或原始 MP3。

## 依赖

本地：

- Python 3.10+
- `numpy`、`librosa`、`soundfile`
- `ffmpeg`、`ffplay`、`fluidsynth`
- SoundFont，默认 `/usr/share/soundfonts/FluidR3_GM.sf2`
- `ssh`、`scp`、`tar`

安装 Python 依赖：

```bash
python3 -m pip install -r projects/igem-pv/requirements.txt
```

远程 Aria-AMT 需要：

- Aria-AMT 源码，当前验证版本为提交 `a1ab73f`；
- 应用 [aria-amt-cpu.patch](aria-amt-cpu.patch)；
- 包含 `torch`、`torchaudio`、`ariautils`、`librosa` 等的 Python 环境；
- `checkpoints/piano-medium-double-1.0.safetensors`。

远程 CPU 补丁的核心改动：

```bash
cd /path/to/aria-amt
git checkout a1ab73f
git apply /path/to/projects/igem-pv/aria-amt-cpu.patch
```

如果远程环境尚无模型权重：

```bash
wget -O checkpoints/piano-medium-double-1.0.safetensors \
  'https://huggingface.co/datasets/loubb/aria-midi/resolve/main/piano-medium-double-1.0.safetensors?download=true'
```

## 文件

- `igem_music.py`：音量、循环检测、循环渲染、播放。
- `presentation_mix.py`：把选定的循环扩展成 15 分钟演示音轨。
- `presentation_background.py`：严格拍点循环、段落留空隙、按旁白音量校准的背景版。
- `presentation_voice_loop.py`：把测试人声循环到 15 分钟，生成人声/背景/混合三个音量对照版本。
- `render-presentation.sh`：一键重建背景和 15 分钟人声循环对照版。
- `play-full-mix.sh`：播放完整 15 分钟背景 + 人声混合版。
- `presentation_isolate.py`：导出硬件背景、人声参考，以及二者同时混合版。
- `prep_piano_stems.sh`：用 Demucs 批量提取钢琴 stem。
- `optimize_midi.py`：量化、清理短音/低力度、同音高重叠和踏板。
- `transcribe.sh`：上传音频到远程，多进程并行转录，下载 MIDI。支持 `--audio-ext wav`。
- `remote_run_cpu.py`：远程单曲 CPU 转录器，由 `transcribe.sh` 自动上传。
- `aria-amt-cpu.patch`：Aria-AMT CPU 补丁。
- `presets/igem-2026.json`：本次五首曲子的主循环选择。
- `presets/igem-2026-candidates.json`：`detect` 生成的候选列表，供替换主候选时参考。
- `presets/presentation-2026.json`：15 分钟演示音轨的分段、循环和音色配置。
- `presets/presentation-background-2026.json`：严格循环背景版的重复次数、空隙和电平配置。
- `presets/presentation-voice-loop-2026.json`：人声循环时长、背景增益、循环淡化和混合限幅配置。
- `requirements.txt`：本地 Python 依赖。

## 1. 远程转录

```bash
cd /home/cjc/Repository/music
projects/igem-pv/transcribe.sh
```

默认：

- 源目录：`/home/cjc/Downloads/iGEM 配乐`
- 本地 MIDI：`.../aria-amt-midi`
- 远程：`igem`，Aria-AMT 根目录 `/home/igem/projects/aria-amt`
- 5 个后台 worker，每个 3 个线程

常用参数：

```bash
projects/igem-pv/transcribe.sh \
  --audio-dir "/home/cjc/Downloads/iGEM 配乐" \
  --midi-dir "/home/cjc/Downloads/iGEM 配乐/aria-amt-midi" \
  --threads 3 \
  --jobs 5 \
  --force
```

不加 `--force` 时，远程已存在同名 MIDI 的曲目会跳过。

## 2. 统一峰值音量

```bash
python3 projects/igem-pv/igem_music.py normalize
```

输出 `loop-preview/gains.json`，其中包含：

- MP3 的 `mp3_gain`（FFmpeg 线性音量）；
- MIDI 的 `midi_gain`（FluidSynth `-g`）；
- 测量用的 MIDI 渲染 WAV。

默认目标峰值是 `-2.0 dBFS`。当前结果：

| 曲目 | MP3 gain | MIDI gain |
|---|---:|---:|
| 开头 | 1.0000 | 5.7544 |
| NEST开始后 | 1.3335 | 2.6607 |
| 2 | 1.6596 | 1.9724 |
| 3 | 0.8318 | 4.7863 |
| 结尾 | 0.7586 | 1.5849 |

## 3. 检测循环候选

```bash
python3 projects/igem-pv/igem_music.py detect
```

默认输出 `loop-preview/candidates.json`；仓库内保留了本次的
`presets/igem-2026-candidates.json`。候选按以下指标评分：

- 循环起点/终点的和声相似度；
- 边界能量跳变；
- 边界 onset 强度；
- 与曲目中间位置的距离。

默认搜索 4 小节和 8 小节候选，仅在曲目中段选择。当前预设文件是：

```text
projects/igem-pv/presets/igem-2026.json
```

每首本次只选用一个主候选；其他候选可以手动替换到该文件。

## 4. 渲染循环预览

```bash
python3 projects/igem-pv/igem_music.py render
```

输出：

```text
/home/cjc/Downloads/iGEM 配乐/loop-preview/<曲名>_loop.wav
/home/cjc/Downloads/iGEM 配乐/loop-preview/markers.json
```

渲染结构：

```text
原曲 0 → 循环起点
循环段 1
循环段 2
循环终点 → 原曲结束
```

接缝处理：

- 循环尾部与下一次循环头部使用**等功率交叉淡化**；
- 起点段和尾段沿用原音频连续位置；
- 默认 150 ms，可按曲目在预设中调整；
- 输出统一峰值 `-2.0 dBFS`。

当前预设：

| 曲目 | 循环开始 | 循环结束 | 长度 | fade |
|---|---:|---:|---:|---:|
| 开头 | 31.277s | 50.480s | 19.203s | 150 ms |
| NEST开始后 | 44.072s | 56.866s | 12.794s | 100 ms |
| 2 | 43.886s | 64.459s | 20.573s | 120 ms |
| 3 | 61.974s | 76.115s | 14.141s | 150 ms |
| 结尾 | 54.846s | 74.048s | 19.202s | 150 ms |

## 5. 播放

播放循环预览：

```bash
python3 projects/igem-pv/igem_music.py play --mode loops
```

后台播放：

```bash
python3 projects/igem-pv/igem_music.py play --mode loops --background
```

其他模式：

```bash
# MIDI 与 MP3 交替：MIDI 开头 → MP3 开头 → MIDI NEST → ...
python3 projects/igem-pv/igem_music.py play --mode normalized

# 只播原始 MP3
python3 projects/igem-pv/igem_music.py play --mode originals

# 只播 MIDI，使用 gains.json 中的 MIDI gain
python3 projects/igem-pv/igem_music.py play --mode midi
```

`--list` 只打印将要执行的命令，不发声：

```bash
python3 projects/igem-pv/igem_music.py play --mode normalized --list
```

播放脚本和日志会写入 `loop-preview/play-<mode>.sh` 与 `.log`。

## 说明

Aria-AMT 是钢琴转录模型。完整混音会主要保留模型识别出的钢琴/键盘部分，不等同于完整编曲分轨。

循环候选的“小节”按 4/4 处理；如果原曲存在变速、弱起或不规则拍号，预设中的时间点应人工复核。


## 6. 15 分钟演示音轨

`presentation_mix.py` 会按配置把每个段落的循环扩展到指定长度，再用 2 秒等功率交叉淡化连接各段。

```bash
python3 projects/igem-pv/presentation_mix.py
```

输出：

```text
/home/cjc/Downloads/iGEM 配乐/presentation/
├── igem-presentation-15min.wav
├── igem-presentation-15min.mp3
└── presentation-cues.json
```

当前分段：

| 段落 | 时间 | 长度 | 来源 | 循环 |
|---|---:|---:|---|---|
| 背景+HP | 0:00–2:20 | 140s | 开头 | 31.277–50.480 |
| SEED 1 | 2:20–4:30 | 130s | NEST开始后 | 44.072–56.866 |
| SEED 2 | 4:30–6:40 | 130s | 2 | 43.886–64.459 |
| SEED 3 | 6:40–9:40 | 180s | 3 | 61.974–76.115 |
| 硬件 | 9:40–12:20 | 160s | 2 | 50.294–60.581 |
| 创业 | 12:20–15:00 | 160s | 结尾 | 54.846–74.048 |

音色处理：

- `calm`：低通 9 kHz + 高通 30 Hz，略降电平；
- `hard`：高通 45 Hz + 3:1 压缩，保持瞬态；
- `distant`：低通 4.2 kHz + 短回声，电平更低。


## 7. 严格循环 + 旁白音量参考版

先从测试旁白中提取音频并测量整体响度：

```bash
python3 projects/igem-pv/presentation_background.py
```

当前参考：

- `test-sound.mp4` 提取音频：`presentation/test-sound.wav`
- 旁白整体响度：约 **-19.2 LUFS**
- 背景目标：旁白下方 **16 dB**，即 **-35.2 LUFS**
- 每个乐段都包含 **起始段 + 完整循环重复 + 结束段**：
  - 起始段：原曲 `0 → loop_start`
  - 循环段：`loop_start → loop_end` 完整重复
  - 结束段：原曲 `loop_end → 原曲结束`
- 段落之间留 **1.75 秒静音**
- 循环接缝仅做 5 ms 边缘淡化，不做改变时间轴的交叉淡化
- 15 分钟总长保持 900 秒

当前严格循环背景的各段结构：

| 段落 | 起始段 | 循环 | 结束段 | 音乐长度 |
|---|---:|---:|---:|---:|
| 背景+HP | 0–31.277 | 31.277–50.480 ×3 | 50.480–81.633 | 120.039s |
| SEED 1 | 0–44.072 | 44.072–56.866 ×4 | 56.866–96.026 | 134.408s |
| SEED 2 | 0–43.886 | 43.886–64.459 ×3 | 64.459–118.047 | 159.193s |
| SEED 3 | 0–61.974 | 61.974–76.115 ×4 | 76.115–118.224 | 160.647s |
| 硬件 | 0–50.294 | 50.294–60.581 ×4 | 60.581–118.047 | 148.908s |
| 创业 | 0–54.846 | 54.846–74.048 ×4 | 74.048–110.446 | 168.052s |

输出：

```text
presentation/igem-presentation-background-15min.wav
presentation/igem-presentation-background-15min.mp3
presentation/presentation-level-test.mp3
presentation/presentation-background-cues.json
```

其中 `presentation-level-test.mp3` 是前 2 分 25 秒的旁白 + 背景试听，用来确认人声与背景比例。


## 8. 15 分钟人声循环音量对照

把 `test-sound.wav` 循环到 15 分钟，同时输出：

- 人声单独循环；
- 提高 10 dB 后的背景；
- 人声 + 提高 10 dB 背景的混合版。

命令：

```bash
python3 projects/igem-pv/presentation_voice_loop.py --background-gain-db 10
```

一键重建当前背景和对照版：

```bash
projects/igem-pv/render-presentation.sh
```

`presentation_voice_loop.py` 在 `test-sound.wav` 不存在时，会自动从
`presets/presentation-voice-loop-2026.json` 中的 `voice_mp4` 提取。

输出：

```text
presentation/presentation-voice-loop-15min.mp3
presentation/presentation-background-plus10db.mp3
presentation/presentation-voice-loop-with-background-15min.mp3
presentation/presentation-voice-loop-levels.json
```

当前测量：

- 人声循环：`-19.2 LUFS`，true peak `-0.6 dBFS`
- 背景 +10 dB：`-25.2 LUFS`，true peak `-11.6 dBFS`
- 混合：`-19.7 LUFS`，true peak `-1.0 dBFS`

如果人声仍然偏大，可以继续：

```bash
python3 projects/igem-pv/presentation_voice_loop.py --background-gain-db 12
```


## 9. 硬件背景与人声同时播放

```bash
python3 projects/igem-pv/presentation_isolate.py --play
```

输出：

```text
presentation/presentation-hardware-background-only.mp3
presentation/presentation-voice-reference.mp3
presentation/presentation-hardware-background-with-voice.mp3
presentation/presentation-isolated-parts.json
```

同时播放版本：

```text
硬件段背景（148.908 秒） + 人声参考（145.408 秒）
```

- 两轨都从 0 秒开始对齐。
- 人声结束后保留约 3.5 秒硬件背景尾段。
- 混合峰值限制在 `-1.0 dBFS`。
- 硬件段背景取自当前 `presentation-background-plus10db.wav` 的
  `581.287–730.195s`，人声参考取自原始 `test-sound.wav`。

## 10. 固定响度表

`presentation_loudness.py` 始终输出以下列：

```text
段落 | 起始段 | 循环 | 结束段 | 音乐长度 | 段后空隙 | 人声 LUFS | 背景 LUFS | 差值
```

命令：

```bash
python3 projects/igem-pv/presentation_loudness.py
```

计算内容：

- 起始段、循环、结束段来自 `presentation-background-cues.json`；
- 每个段落的人声/背景 LUFS 按该段的 `music_start_s` 与 `music_duration_s` 实测；
- 差值统一为 `人声 LUFS - 背景 LUFS`；
- 额外输出整体 LUFS 和按段落时长加权的平均差值。

输出：

```text
presentation/presentation-loudness-report.md
presentation/presentation-loudness-report.json
```
