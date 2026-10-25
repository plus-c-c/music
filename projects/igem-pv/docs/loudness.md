# 人声与背景响度

## 测量口径

- `LUFS`：EBU R128 integrated loudness，用 `ffmpeg ebur128` 实测。
- `dBFS`：采样峰值。
- 每个展示段的人声/背景 LUFS 都按该段的 `music_start_s` 和
  `music_duration_s` 实测。
- 差值统一为：

```text
人声 LUFS - 背景 LUFS
```

注意：分段 LUFS 差值不等于整体 LUFS 差值，也不等于分段 RMS 差值。
报告中统一使用 LUFS。

## 当前基准

- 人声测试：`test-sound.wav`
- 人声整体响度：约 **-19.2 LUFS**
- 人声峰值：约 **-0.6 dBFS**
- 背景基础版：约 **-35.2 LUFS**
- 用于对照的背景增益：**+10 dB**
- 对照背景响度：约 **-25.2 LUFS**
- 人声 + 背景混音峰值：限制在 **-1.0 dBFS**

## 固定表格

该表格必须始终包含以下列：

```text
段落 | 起始段 | 循环 | 结束段 | 音乐长度 | 段后空隙 |
人声 LUFS | 背景 LUFS | 差值
```

生成命令：

```bash
python3 presentation_loudness.py
```

输出：

```text
/home/cjc/Downloads/iGEM 配乐/presentation/presentation-loudness-report.md
/home/cjc/Downloads/iGEM 配乐/presentation/presentation-loudness-report.json
```

当前快照保存在：

```text
docs/current-loudness-report.md
```

## 电平处理

- `calm`：低通 9 kHz + 高通 30 Hz，略降电平。
- `hard`：高通 45 Hz + 3:1 压缩。
- `distant`：低通 4.2 kHz + 短回声，降低约 4 dB。
- 背景基础版先统一到 `-35.2 LUFS`；对照版整体 +10 dB。
- 同时播放人声和背景时，峰值限制在 `-1.0 dBFS`，避免削波。
