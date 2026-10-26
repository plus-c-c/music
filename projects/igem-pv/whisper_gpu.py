#!/usr/bin/env python3
"""Run OpenAI Whisper on the AMD ROCm GPU.

This is the PyTorch Whisper path.  The existing faster-whisper models in
/home/igem/asr/models are CTranslate2 format and use a CTranslate2 build that
only supports NVIDIA CUDA; they cannot use the AMD GPU directly.
"""

from __future__ import annotations

import argparse
import json
import math
import os
import sys
import time
from pathlib import Path

import librosa
import numpy as np
import soundfile as sf

DEFAULT_MODEL = "large-v3"
DEFAULT_MODEL_DIR = Path("/home/igem/asr/whisper-models")


def load_audio(path: Path, sr: int = 16000) -> np.ndarray:
    data, source_sr = sf.read(str(path), always_2d=True, dtype="float32")
    if data.shape[1] > 1:
        data = data.mean(axis=1)
    else:
        data = data[:, 0]
    if source_sr != sr:
        data = librosa.resample(
            data, orig_sr=source_sr, target_sr=sr, res_type="soxr_hq"
        )
    return np.ascontiguousarray(data, dtype=np.float32)


def format_timestamp(seconds: float) -> str:
    ms = int(round(seconds * 1000))
    h, ms = divmod(ms, 3600000)
    m, ms = divmod(ms, 60000)
    s, ms = divmod(ms, 1000)
    return f"{h:02d}:{m:02d}:{s:02d},{ms:03d}"


def write_srt(segments: list[dict], path: Path) -> None:
    lines = []
    for i, seg in enumerate(segments, 1):
        lines += [
            str(i),
            f"{format_timestamp(float(seg['start']))} --> "
            f"{format_timestamp(float(seg['end']))}",
            str(seg["text"]).strip(),
            "",
        ]
    path.write_text("\n".join(lines), encoding="utf-8")


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Transcribe audio with OpenAI Whisper on AMD ROCm"
    )
    parser.add_argument("audio", help="input audio file (WAV/FLAC/MP3)")
    parser.add_argument("--model", default=DEFAULT_MODEL)
    parser.add_argument("--model-dir", default=str(DEFAULT_MODEL_DIR))
    parser.add_argument("--device", default="cuda")
    parser.add_argument("--language", default=None)
    parser.add_argument("--output", default=None)
    parser.add_argument("--word-timestamps", action="store_true")
    parser.add_argument("--no-fp16", action="store_true")
    args = parser.parse_args()
    try:
        import torch
        import whisper
    except ImportError as exc:
        print(f"error: Whisper GPU dependencies missing: {exc}", file=sys.stderr)
        print("run this script with the ROCm Whisper environment on iGEM", file=sys.stderr)
        return 2
    audio_path = Path(args.audio)
    if not audio_path.is_file():
        print(f"error: missing audio: {audio_path}", file=sys.stderr)
        return 2
    if args.device == "cuda" and not torch.cuda.is_available():
        print("error: torch.cuda is not available in this environment", file=sys.stderr)
        return 2
    model_dir = Path(args.model_dir)
    model_dir.mkdir(parents=True, exist_ok=True)
    print(f"loading {args.model} on {args.device} ...", flush=True)
    model = whisper.load_model(
        args.model,
        device=args.device,
        download_root=str(model_dir),
    )
    audio = load_audio(audio_path)
    started = time.time()
    result = model.transcribe(
        audio,
        language=args.language,
        fp16=not args.no_fp16,
        word_timestamps=args.word_timestamps,
        verbose=False,
    )
    if torch.cuda.is_available():
        torch.cuda.synchronize()
    elapsed = time.time() - started
    output = Path(args.output) if args.output else audio_path.with_suffix(".json")
    payload = {
        "audio": str(audio_path),
        "model": args.model,
        "device": str(next(model.parameters()).device),
        "elapsed_s": round(elapsed, 3),
        "language": result.get("language"),
        "text": result.get("text", "").strip(),
        "segments": result.get("segments", []),
    }
    output.write_text(json.dumps(payload, ensure_ascii=False, indent=2), encoding="utf-8")
    write_srt(result.get("segments", []), output.with_suffix(".srt"))
    (output.with_suffix(".txt")).write_text(
        payload["text"] + "\n", encoding="utf-8"
    )
    print(
        f"device={payload['device']} elapsed={elapsed:.2f}s "
        f"language={payload['language']} segments={len(payload['segments'])}"
    )
    print(payload["text"])
    print(f"wrote {output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
