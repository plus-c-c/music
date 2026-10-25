#!/usr/bin/env python3
"""Build a 15-minute voice-loop level comparison.

The source voice test is looped to 15 minutes so its level can be compared
against the full presentation background.  The background is raised by a
configurable number of dB (default +6) and mixed without changing the voice.
"""

from __future__ import annotations

import argparse
import json
import math
import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

import numpy as np
import soundfile as sf

HERE = Path(__file__).resolve().parent
DEFAULT_AUDIO_DIR = Path(
    os.environ.get("IGEM_AUDIO_DIR", "/home/cjc/Downloads/iGEM 配乐")
)
DEFAULT_VOICE = DEFAULT_AUDIO_DIR / "presentation" / "test-sound.wav"
DEFAULT_BACKGROUND = (
    DEFAULT_AUDIO_DIR
    / "presentation"
    / "igem-presentation-background-15min.wav"
)
DEFAULT_OUT_DIR = DEFAULT_AUDIO_DIR / "presentation"
SR = 48000


class MixError(RuntimeError):
    pass


def require(name: str) -> None:
    if not shutil.which(name):
        raise MixError(f"required executable not found: {name}")


def equal_power(a: np.ndarray, b: np.ndarray) -> np.ndarray:
    n = min(len(a), len(b))
    if n <= 0:
        return np.empty((0, a.shape[1]))
    t = np.linspace(0.0, np.pi / 2.0, n, endpoint=False)
    return a[-n:] * np.cos(t)[:, None] + b[:n] * np.sin(t)[:, None]


def loop_to_length(
    source: np.ndarray,
    target_s: float,
    crossfade_ms: float = 100.0,
) -> np.ndarray:
    """Loop a source with equal-power seam crossfades, then trim/pad."""
    target = int(round(target_s * SR))
    if len(source) >= target:
        return source[:target].copy()
    d = max(1, min(int(round(crossfade_ms * SR / 1000.0)), len(source) // 4))
    out = source.copy()
    while len(out) < target:
        xf = equal_power(out[-d:], source[:d])
        out = np.concatenate([out[:-d], xf, source[d:]], axis=0)
    out = out[:target].copy()
    fade = min(int(1.0 * SR), len(out))
    out[-fade:] *= np.linspace(1.0, 0.0, fade)[:, None]
    return out


def encode_mp3(wav: Path, mp3: Path) -> None:
    p = subprocess.run(
        [
            "ffmpeg",
            "-y",
            "-hide_banner",
            "-loglevel",
            "error",
            "-i",
            str(wav),
            "-codec:a",
            "libmp3lame",
            "-b:a",
            "320k",
            str(mp3),
        ],
        capture_output=True,
        text=True,
    )
    if p.returncode != 0:
        raise MixError(f"MP3 encode failed:\n{p.stderr[-2000:]}")


def db_gain(db: float) -> float:
    return 10.0 ** (db / 20.0)


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Loop the voice test over 15 minutes for level comparison"
    )
    parser.add_argument("--voice", default=str(DEFAULT_VOICE))
    parser.add_argument("--background", default=str(DEFAULT_BACKGROUND))
    parser.add_argument("--out-dir", default=str(DEFAULT_OUT_DIR))
    parser.add_argument("--duration-s", type=float, default=900.0)
    parser.add_argument(
        "--background-gain-db",
        type=float,
        default=6.0,
        help="overall background boost relative to the current background stem",
    )
    parser.add_argument(
        "--voice-gain-db",
        type=float,
        default=0.0,
        help="voice gain; default keeps the original voice level",
    )
    parser.add_argument(
        "--voice-loop-crossfade-ms",
        type=float,
        default=100.0,
    )
    parser.add_argument(
        "--mix-peak-dbfs",
        type=float,
        default=-1.0,
        help="only scale the mix down if its peak exceeds this",
    )
    args = parser.parse_args()
    require("ffmpeg")
    voice_path = Path(args.voice)
    bg_path = Path(args.background)
    if not voice_path.is_file():
        raise MixError(f"missing voice WAV: {voice_path}")
    if not bg_path.is_file():
        raise MixError(f"missing background WAV: {bg_path}")
    out_dir = Path(args.out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)

    voice, voice_sr = sf.read(
        str(voice_path), always_2d=True, dtype="float64"
    )
    bg, bg_sr = sf.read(str(bg_path), always_2d=True, dtype="float64")
    if voice_sr != SR or bg_sr != SR:
        raise MixError(f"expected {SR} Hz, got voice={voice_sr}, bg={bg_sr}")
    voice_loop = loop_to_length(
        voice,
        float(args.duration_s),
        float(args.voice_loop_crossfade_ms),
    )
    bg = bg[: len(voice_loop)]
    if len(bg) < len(voice_loop):
        bg = np.concatenate(
            [bg, np.zeros((len(voice_loop) - len(bg), 2))], axis=0
        )

    vg = db_gain(float(args.voice_gain_db))
    bg_gain = db_gain(float(args.background_gain_db))
    voice_only = np.clip(voice_loop * vg, -1.0, 1.0)
    bg_boosted = np.clip(bg * bg_gain, -1.0, 1.0)
    mix = voice_only + bg_boosted
    peak = float(np.max(np.abs(mix))) if len(mix) else 0.0
    peak_limit = db_gain(float(args.mix_peak_dbfs))
    if peak > peak_limit:
        mix *= peak_limit / peak
    mix = np.clip(mix, -1.0, 1.0)

    voice_wav = out_dir / "presentation-voice-loop-15min.wav"
    voice_mp3 = out_dir / "presentation-voice-loop-15min.mp3"
    bg_wav = out_dir / f"presentation-background-plus{args.background_gain_db:g}db.wav"
    bg_mp3 = out_dir / f"presentation-background-plus{args.background_gain_db:g}db.mp3"
    mix_wav = out_dir / "presentation-voice-loop-with-background-15min.wav"
    mix_mp3 = out_dir / "presentation-voice-loop-with-background-15min.mp3"
    sf.write(str(voice_wav), voice_only, SR, subtype="PCM_16")
    encode_mp3(voice_wav, voice_mp3)
    sf.write(str(bg_wav), bg_boosted, SR, subtype="PCM_16")
    encode_mp3(bg_wav, bg_mp3)
    sf.write(str(mix_wav), mix, SR, subtype="PCM_16")
    encode_mp3(mix_wav, mix_mp3)

    def peak_dbfs(x: np.ndarray) -> float:
        return 20.0 * math.log10(float(np.max(np.abs(x))) + 1e-12)

    info = {
        "duration_s": round(len(voice_loop) / SR, 3),
        "voice_source": str(voice_path),
        "background_source": str(bg_path),
        "voice_gain_db": args.voice_gain_db,
        "background_gain_db": args.background_gain_db,
        "voice_loop_crossfade_ms": args.voice_loop_crossfade_ms,
        "voice_peak_dbfs": round(peak_dbfs(voice_only), 3),
        "background_peak_dbfs": round(peak_dbfs(bg_boosted), 3),
        "mix_peak_dbfs": round(peak_dbfs(mix), 3),
        "voice_mp3": str(voice_mp3),
        "background_mp3": str(bg_mp3),
        "mix_mp3": str(mix_mp3),
    }
    (out_dir / "presentation-voice-loop-levels.json").write_text(
        json.dumps(info, ensure_ascii=False, indent=2)
    )
    print(json.dumps(info, ensure_ascii=False, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
