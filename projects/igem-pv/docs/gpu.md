# iGEM 服务器 GPU

## 硬件

```text
GPU:      AMD Radeon RX 9070 XT
架构:     gfx1201 (RDNA4)
ROCm:     7.1
显存:     16 GB
```

PyTorch ROCm 仍通过 `torch.cuda` API 暴露设备；在脚本中写
`--device cuda` 表示使用 AMD ROCm GPU。

## Demucs

独立环境：

```text
/home/igem/.local/miniconda3/envs/demucs-rocm/bin/python
```

```text
torch       2.11.0+rocm7.1
torchaudio  2.11.0+rocm7.1
demucs      4.1.0
```

默认使用 GPU：

```bash
projects/igem-pv/prep_piano_stems_remote.sh \
  --audio-dir "/home/cjc/Downloads/iGEM 配乐" \
  --tracks 开头,NEST开始后,2,3,结尾
```

CPU 回退：

```bash
projects/igem-pv/prep_piano_stems_remote.sh \
  --device cpu \
  --remote-python /home/igem/.local/miniconda3/envs/demucs/bin/python
```

已验证 8 秒片段：GPU 分离耗时约 5 秒，输出 `piano.wav` 正常。

## Aria-AMT GPU

GPU 源码：

```text
/home/igem/projects/aria-amt-gpu
```

它使用同一套 ROCm 环境：

```text
/home/igem/.local/miniconda3/envs/demucs-rocm/bin/python
```

由于 ROCm 的 torchaudio 2.11 移除了 `torchaudio.io.StreamReader`，GPU 源码
应用了 [aria-amt-gpu-audio.patch](aria-amt-gpu-audio.patch)，改用 librosa
读取整段音频后按 30 秒窗口切分。

转录：

```bash
projects/igem-pv/transcribe_gpu.sh \
  --audio-dir "/home/cjc/Downloads/iGEM 配乐" \
  --audio-ext mp3 \
  --midi-dir "/home/cjc/Downloads/iGEM 配乐/aria-amt-midi-gpu"
```

已验证：

- AMD Radeon RX 9070 XT
- `torch 2.11.0+rocm7.1`
- Aria-AMT `medium-double`
- 短片段 GPU 转录成功并生成 MIDI

## Whisper GPU

现有的：

```text
/home/igem/asr/models/faster-whisper-large-v3
/home/igem/asr/models/faster-whisper-small
/home/igem/asr/venv
```

是 **faster-whisper / CTranslate2** 格式。当前 CTranslate2 4.8.3
只支持 NVIDIA CUDA，`ctranslate2.get_cuda_device_count()` 返回 0，
不能直接使用 AMD GPU。

因此增加了 PyTorch Whisper GPU 路径：

```text
whisper_gpu.py
```

环境仍是：

```text
/home/igem/.local/miniconda3/envs/demucs-rocm/bin/python
```

使用：

```bash
/home/igem/.local/miniconda3/envs/demucs-rocm/bin/python \
  projects/igem-pv/whisper_gpu.py \
  "/path/to/audio.wav" \
  --model large-v3 \
  --language en
```

输出同目录下的 `.json`、`.srt` 和 `.txt`。

模型缓存：

```text
/home/igem/asr/whisper-models/
```

已验证 `tiny` 模型在 AMD GPU 上运行，`device=cuda:0`，10 秒音频约 1 秒完成；
`openai-whisper` 的 `large-v3` 首次使用会自动下载到上述缓存目录。
