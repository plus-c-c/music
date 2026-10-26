# MIDI 优化流程

## 先判断目标

- **循环/编曲参考**：现有音频分析已经足够，MIDI 不是主依据。
- **可编辑 MIDI/总谱**：先分轨，再做转录和后处理。
- **谱面级准确度**：最终仍需人工修正音高、节奏、声部和踏板。

## 1. 先分离钢琴

Aria-AMT 是钢琴转录模型，完整混音会引入鼓、贝斯、人声和合成器误判。

推荐在 iGEM 服务器上运行 Demucs：

```bash
cd /home/cjc/Repository/music
projects/igem-pv/prep_piano_stems_remote.sh \
  --audio-dir "/home/cjc/Downloads/iGEM 配乐" \
  --tracks 开头,NEST开始后,2,3,结尾
```

远程环境：

```text
/home/igem/.local/miniconda3/envs/demucs/bin/python
```

远程 Demucs 环境已安装：

```bash
python3 -m pip install torch==2.5.1 torchaudio==2.5.1 \
  --index-url https://download.pytorch.org/whl/cpu
python3 -m pip install -r requirements-demucs.txt
```

本地运行也支持，但需要本机安装 Demucs：

```bash
projects/igem-pv/prep_piano_stems.sh
```

默认使用：

```text
Demucs htdemucs_6s
```

输出：

```text
/home/cjc/Downloads/iGEM 配乐/stems/piano/*.wav
```

需要先安装：

```bash
python3 -m pip install demucs
```

CPU 分离会比较慢；有 GPU 时可以加：

```bash
--device cuda
```

## 2. 转录钢琴 stem

```bash
projects/igem-pv/transcribe.sh \
  --audio-dir "/home/cjc/Downloads/iGEM 配乐/stems/piano" \
  --audio-ext wav \
  --midi-dir "/home/cjc/Downloads/iGEM 配乐/aria-amt-midi-piano" \
  --remote-output /home/igem/projects/aria-amt/output/iGEM-piano
```

输出到：

```text
aria-amt-midi-piano/*.mid
```

## 3. MIDI 后处理

```bash
python3 projects/igem-pv/optimize_midi.py \
  --midi-dir "/home/cjc/Downloads/iGEM 配乐/aria-amt-midi-piano" \
  --out-dir "/home/cjc/Downloads/iGEM 配乐/aria-amt-midi-piano-optimized" \
  --bpm 99.38 \
  --quantize 1/16
```

每个文件独立处理：

- `--bpm`：写入真实速度；可逐曲指定。
- `--quantize 1/16|1/32|none`：只量化 note-on，保留原音符时值。
- `--min-velocity`：默认删除 velocity `< 10` 的音符。
- `--min-duration-ms`：默认删除短于 `30 ms` 的音符。
- 默认裁剪同音高重叠音符。
- 删除重复 CC64，并在曲尾抬起踏板。
- 输出 `optimize-report.json`。

如果原曲有 rubato 或不规则速度，不建议用全局 `--bpm` 量化；
应先建立 tempo map，再做局部量化。

## 4. 校对

- 用 FluidSynth 渲染并 A/B 原音频。
- 在钢琴卷帘中检查：漏音、多音、八度误判、踏板断点。
- 有参考 MIDI 时可用 `mir_eval` 的 note F1。
- 没有参考时，优先保证低音、和弦根音、旋律和踏板正确。
