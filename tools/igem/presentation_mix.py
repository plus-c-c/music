#!/usr/bin/env python3
"""Render a 15-minute presentation soundtrack from looped source material.

The arrangement is defined in presets/presentation-2026.json.  Each section
is built by extending a selected source loop to the section duration, then the
sections are joined with an equal-power crossfade.  A short crossfade is also
used at every internal loop seam.
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
DEFAULT_CONFIG = HERE / "presets" / "presentation-2026.json"
DEFAULT_OUT_DIR = DEFAULT_AUDIO_DIR / "presentation"
SR = 48000


class MixError(RuntimeError):
    pass


def require(name: str) -> None:
    if not shutil.which(name):
        raise MixError(f"required executable not found: {name}")


def decode_mp3(path: Path, out: Path) -> None:
    require("ffmpeg")
    out.parent.mkdir(parents=True, exist_ok=True)
    p = subprocess.run(
        [
            "ffmpeg",
            "-y",
            "-hide_banner",
            "-loglevel",
            "error",
            "-i",
            str(path),
            "-ar",
            str(SR),
            "-ac",
            "2",
            "-c:a",
            "pcm_f32le",
            str(out),
        ],
        capture_output=True,
        text=True,
    )
    if p.returncode != 0:
        raise MixError(f"could not decode {path}:\n{p.stderr[-2000:]}")


def equal_power(a: np.ndarray, b: np.ndarray) -> np.ndarray:
    n = min(len(a), len(b))
    if n <= 0:
        return np.empty((0, a.shape[1]))
    t = np.linspace(0.0, np.pi / 2.0, n, endpoint=False)
    return a[-n:] * np.cos(t)[:, None] + b[:n] * np.sin(t)[:, None]


def extend_loop(
    source: np.ndarray,
    start_s: float,
    end_s: float,
    length_s: float,
    seam_ms: float,
) -> np.ndarray:
    """Repeat a source loop until it is long enough, crossfading each seam."""
    s = max(0, int(round(start_s * SR)))
    e = min(len(source), int(round(end_s * SR)))
    if e <= s:
        raise MixError(f"invalid loop: {start_s} -> {end_s}")
    loop = source[s:e]
    target = int(round(length_s * SR))
    if len(loop) >= target:
        return loop[:target].copy()
    d = max(1, min(int(round(seam_ms * SR / 1000.0)), len(loop) // 4))
    out = loop.copy()
    while len(out) < target:
        xf = equal_power(out[-d:], loop[:d])
        out = np.concatenate([out[:-d], xf, loop[d:]], axis=0)
    return out[:target].copy()


def apply_tone(
    audio: np.ndarray,
    tone: str,
    length_s: float,
    tmpdir: Path,
) -> np.ndarray:
    """Apply a deliberate tone colour through FFmpeg."""
    if tone == "calm":
        af = "highpass=f=30,lowpass=f=9000,volume=-1.5dB"
    elif tone == "hard":
        af = (
            "highpass=f=45,"
            "acompressor=threshold=0.10:ratio=3:attack=3:release=80,"
            "volume=0dB"
        )
    elif tone == "distant":
        af = "lowpass=f=4200,aecho=0.8:0.55:120:0.35,volume=-4dB"
    else:
        raise MixError(f"unknown tone: {tone}")

    src = tmpdir / f"tone-{tone}.wav"
    dst = tmpdir / f"tone-{tone}-out.wav"
    sf.write(str(src), np.clip(audio, -1.0, 1.0), SR, subtype="PCM_24")
    p = subprocess.run(
        [
            "ffmpeg",
            "-y",
            "-hide_banner",
            "-loglevel",
            "error",
            "-i",
            str(src),
            "-af",
            af,
            "-t",
            f"{length_s:.6f}",
            "-ar",
            str(SR),
            "-ac",
            "2",
            "-c:a",
            "pcm_f32le",
            str(dst),
        ],
        capture_output=True,
        text=True,
    )
    if p.returncode != 0:
        raise MixError(f"tone processing failed ({tone}):\n{p.stderr[-2000:]}")
    data, rate = sf.read(str(dst), always_2d=True, dtype="float64")
    if rate != SR:
        raise MixError(f"unexpected processed sample rate: {rate}")
    return data


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


def build(config: dict, audio_dir: Path, out_dir: Path) -> dict:
    require("ffmpeg")
    sections = config.get("sections", [])
    if not sections:
        raise MixError("config has no sections")
    target = float(config.get("target_dbfs", -2.0))
    section_xf = float(config.get("section_crossfade_s", 2.0))
    seam_ms = float(config.get("loop_seam_ms", 120.0))
    total_target = sum(float(s["duration_s"]) for s in sections)
    out_dir.mkdir(parents=True, exist_ok=True)

    with tempfile.TemporaryDirectory(prefix="igem-mix-", dir=str(out_dir)) as td:
        tmpdir = Path(td)
        decoded: dict[str, Path] = {}
        for section in sections:
            name = section["source"]
            if name not in decoded:
                raw = audio_dir / f"{name}.mp3"
                if not raw.is_file():
                    raise MixError(f"missing source MP3: {raw}")
                out = tmpdir / f"source-{name}.wav"
                decode_mp3(raw, out)
                decoded[name] = out

        rendered = []
        n = len(sections)
        for idx, section in enumerate(sections):
            name = section["source"]
            target_s = float(section["duration_s"])
            # The first n-1 sections carry the overlap needed by the
            # equal-power section crossfade; the final section does not.
            raw_s = target_s + (section_xf if idx < n - 1 else 0.0)
            source, rate = sf.read(
                str(decoded[name]), always_2d=True, dtype="float64"
            )
            if rate != SR:
                raise MixError(f"unexpected source sample rate: {rate}")
            looped = extend_loop(
                source,
                float(section["loop_start_s"]),
                float(section["loop_end_s"]),
                raw_s,
                seam_ms,
            )
            processed = apply_tone(
                looped,
                str(section["tone"]),
                raw_s,
                tmpdir,
            )
            # Level the sections by RMS before the final peak normalization.
            # Calm material sits below the hard hardware section, and the
            # entrepreneurship section is deliberately farther back.
            target_rms = float(
                section.get(
                    "rms_dbfs",
                    {"calm": -22.0, "hard": -20.0, "distant": -26.0}[
                        section["tone"]
                    ],
                )
            )
            rms = float(np.sqrt(np.mean(processed * processed)))
            if rms > 1e-9:
                processed *= 10.0 ** (
                    (target_rms - 20.0 * math.log10(rms)) / 20.0
                )
            peak = float(np.max(np.abs(processed)))
            if peak > 0.98:
                processed *= 0.98 / peak
            rendered.append(processed)
            print(
                f"{section['name']}: {target_s:.0f}s "
                f"from {name} "
                f"[{section['loop_start_s']:.3f}-{section['loop_end_s']:.3f}] "
                f"{section['tone']}",
                flush=True,
            )

        mix = rendered[0]
        d = int(round(section_xf * SR))
        for nxt in rendered[1:]:
            if d <= 0:
                mix = np.concatenate([mix, nxt], axis=0)
            else:
                xf = equal_power(mix[-d:], nxt[:d])
                mix = np.concatenate([mix[:-d], xf, nxt[d:]], axis=0)

        # Soft head/tail only; section joints already have crossfades.
        fade_in = int(1.0 * SR)
        fade_out = int(2.5 * SR)
        if fade_in:
            mix[:fade_in] *= np.linspace(0.0, 1.0, fade_in)[:, None]
        if fade_out:
            mix[-fade_out:] *= np.linspace(1.0, 0.0, fade_out)[:, None]

        peak = float(np.max(np.abs(mix))) if len(mix) else 0.0
        gain = (10.0 ** (target / 20.0)) / peak if peak > 0 else 1.0
        mix = np.clip(mix * gain, -1.0, 1.0)

        wav = out_dir / "igem-presentation-15min.wav"
        mp3 = out_dir / "igem-presentation-15min.mp3"
        sf.write(str(wav), mix, SR, subtype="PCM_16")
        encode_mp3(wav, mp3)

        # Source-relative cue positions.  Because the first n-1 raw sections
        # include the crossfade overlap, final section boundaries are the
        # requested durations.
        cursor = 0.0
        cues = []
        for section in sections:
            dur = float(section["duration_s"])
            cues.append(
                {
                    "name": section["name"],
                    "start_s": round(cursor, 3),
                    "end_s": round(cursor + dur, 3),
                    "duration_s": round(dur, 3),
                    "source": section["source"],
                    "loop_start_s": section["loop_start_s"],
                    "loop_end_s": section["loop_end_s"],
                    "tone": section["tone"],
                    "note": section.get("note", ""),
                }
            )
            cursor += dur

        info = {
            "title": config.get("title", "iGEM presentation"),
            "total_duration_s": round(len(mix) / SR, 3),
            "target_dbfs": target,
            "section_crossfade_s": section_xf,
            "loop_seam_ms": seam_ms,
            "wav": str(wav),
            "mp3": str(mp3),
            "sections": cues,
        }
        (out_dir / "presentation-cues.json").write_text(
            json.dumps(info, ensure_ascii=False, indent=2)
        )
        return info


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Render the 15-minute iGEM presentation soundtrack"
    )
    parser.add_argument("--audio-dir", default=str(DEFAULT_AUDIO_DIR))
    parser.add_argument("--config", default=str(DEFAULT_CONFIG))
    parser.add_argument("--out-dir", default=str(DEFAULT_OUT_DIR))
    parser.add_argument("--list", action="store_true")
    args = parser.parse_args()
    config = json.loads(Path(args.config).read_text())
    if args.list:
        print(json.dumps(config, ensure_ascii=False, indent=2))
        return 0
    try:
        info = build(config, Path(args.audio_dir), Path(args.out_dir))
    except MixError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 2
    print(json.dumps(info, ensure_ascii=False, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
