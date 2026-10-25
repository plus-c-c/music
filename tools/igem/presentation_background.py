#!/usr/bin/env python3
"""Render the strict-loop presentation background.

Differences from presentation_mix.py:
  * Each music section is an exact integer number of source-loop repetitions.
  * Sections are separated by silence gaps instead of crossfades, so the
    section length can flex without changing the loop grid.
  * Loop seams are cut on source beat boundaries and use only a 5 ms edge fade;
    no overlap/crossfade shifts the beat grid.
  * The final background loudness is set relative to the extracted voice test.
"""

from __future__ import annotations

import argparse
import json
import math
import os
import re
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
DEFAULT_CONFIG = HERE / "presets" / "presentation-background-2026.json"
DEFAULT_OUT_DIR = DEFAULT_AUDIO_DIR / "presentation"
SR = 48000


class MixError(RuntimeError):
    pass


def require(name: str) -> None:
    if not shutil.which(name):
        raise MixError(f"required executable not found: {name}")


def decode_audio(src: Path, out: Path) -> None:
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
            str(src),
            "-vn",
            "-ar",
            str(SR),
            "-ac",
            "2",
            "-c:a",
            "pcm_s16le",
            str(out),
        ],
        capture_output=True,
        text=True,
    )
    if p.returncode != 0:
        raise MixError(f"could not decode {src}:\n{p.stderr[-2000:]}")


def measure_lufs(path: Path) -> float:
    require("ffmpeg")
    p = subprocess.run(
        [
            "ffmpeg",
            "-hide_banner",
            "-nostats",
            "-i",
            str(path),
            "-filter_complex",
            "ebur128=peak=true",
            "-f",
            "null",
            "-",
        ],
        capture_output=True,
        text=True,
    )
    if p.returncode != 0:
        raise MixError(f"EBU R128 measurement failed for {path}")
    summary = p.stderr.rsplit("Summary:", 1)[-1]
    m = re.search(
        r"Integrated loudness:\s*\n\s*I:\s*(-?[0-9.]+)\s*LUFS",
        summary,
    )
    if not m:
        raise MixError(f"could not parse integrated loudness for {path}")
    return float(m.group(1))


def apply_tone(audio: np.ndarray, tone: str, duration_s: float, tmpdir: Path):
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
            f"{duration_s:.6f}",
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


def strict_loop(source: np.ndarray, start_s: float, end_s: float, repeats: int, seam_ms: float):
    """Tile the exact loop N times; only tiny edge fades, no timing shift."""
    s = max(0, int(round(start_s * SR)))
    e = min(len(source), int(round(end_s * SR)))
    if e <= s:
        raise MixError(f"invalid loop: {start_s} -> {end_s}")
    loop = source[s:e].copy()
    fade = max(0, min(int(round(seam_ms * SR / 1000.0)), len(loop) // 4))
    if fade:
        loop[:fade] *= np.linspace(0.0, 1.0, fade)[:, None]
        loop[-fade:] *= np.linspace(1.0, 0.0, fade)[:, None]
    return np.concatenate([loop] * int(repeats), axis=0)


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
    total_target = float(config.get("total_duration_s", 900.0))
    gap = float(config.get("section_gap_s", 2.0))
    seam_ms = float(config.get("loop_seam_fade_ms", 5.0))
    edge_ms = float(config.get("section_edge_fade_ms", 10.0))
    voice_mp4 = Path(config["voice_reference"])
    voice_wav = Path(config["voice_reference_wav"])
    out_dir.mkdir(parents=True, exist_ok=True)
    if not voice_mp4.is_file():
        raise MixError(f"missing voice reference: {voice_mp4}")
    if not voice_wav.is_file():
        decode_audio(voice_mp4, voice_wav)
    voice_lufs = measure_lufs(voice_wav)
    target_bg_lufs = voice_lufs - float(
        config.get("voice_to_background_offset_db", 16.0)
    )
    print(f"voice: {voice_lufs:.2f} LUFS; target background: {target_bg_lufs:.2f} LUFS")

    with tempfile.TemporaryDirectory(prefix="igem-bg-", dir=str(out_dir)) as td:
        tmpdir = Path(td)
        decoded: dict[str, Path] = {}
        for sec in sections:
            name = sec["source"]
            if name not in decoded:
                raw = audio_dir / f"{name}.mp3"
                if not raw.is_file():
                    raise MixError(f"missing source MP3: {raw}")
                out = tmpdir / f"source-{name}.wav"
                decode_audio(raw, out)
                decoded[name] = out

        rendered = []
        for sec in sections:
            source, rate = sf.read(
                str(decoded[sec["source"]]), always_2d=True, dtype="float64"
            )
            if rate != SR:
                raise MixError(f"unexpected source sample rate: {rate}")
            repeats = int(sec["loop_repeats"])
            raw_loop = strict_loop(
                source,
                float(sec["loop_start_s"]),
                float(sec["loop_end_s"]),
                repeats,
                seam_ms,
            )
            duration = len(raw_loop) / SR
            processed = apply_tone(raw_loop, str(sec["tone"]), duration, tmpdir)
            target_rms = float(sec["rms_dbfs"])
            rms = float(np.sqrt(np.mean(processed * processed)))
            if rms > 1e-9:
                processed *= 10.0 ** (
                    (target_rms - 20.0 * math.log10(rms)) / 20.0
                )
            # Section edge fades do not affect the internal beat grid.
            fade = max(1, min(int(round(edge_ms * SR / 1000.0)), len(processed) // 4))
            processed[:fade] *= np.linspace(0.0, 1.0, fade)[:, None]
            processed[-fade:] *= np.linspace(1.0, 0.0, fade)[:, None]
            rendered.append(processed)
            print(
                f"{sec['name']}: {repeats} x {duration/repeats:.3f}s = "
                f"{duration:.3f}s, {sec['tone']}",
                flush=True,
            )

        music_total = sum(len(x) for x in rendered) / SR
        gaps_total = gap * (len(rendered) - 1)
        tail = total_target - music_total - gaps_total
        if tail < -0.01:
            raise MixError(
                f"music + gaps exceed total duration: "
                f"{music_total:.3f} + {gaps_total:.3f} > {total_target:.3f}"
            )
        tail = max(0.0, tail)

        pieces = []
        cues = []
        cursor = 0.0
        gap_samples = int(round(gap * SR))
        silence = np.zeros((gap_samples, 2), dtype=np.float64)
        for idx, (sec, audio) in enumerate(zip(sections, rendered)):
            start = cursor
            pieces.append(audio)
            cursor += len(audio) / SR
            end = cursor
            if idx < len(rendered) - 1:
                pieces.append(silence)
                cursor += gap
            loop_len = (
                float(sec["loop_end_s"]) - float(sec["loop_start_s"])
            )
            cues.append(
                {
                    "name": sec["name"],
                    "music_start_s": round(start, 3),
                    "music_end_s": round(end, 3),
                    "music_duration_s": round(end - start, 3),
                    "gap_after_s": gap if idx < len(rendered) - 1 else round(tail, 3),
                    "source": sec["source"],
                    "loop_start_s": sec["loop_start_s"],
                    "loop_end_s": sec["loop_end_s"],
                    "loop_length_s": round(loop_len, 3),
                    "loop_repeats": sec["loop_repeats"],
                    "tone": sec["tone"],
                }
            )
        if tail:
            pieces.append(np.zeros((int(round(tail * SR)), 2), dtype=np.float64))
        mix = np.concatenate(pieces, axis=0)
        # Trim/pad only if sample rounding left a negligible remainder.
        target_samples = int(round(total_target * SR))
        if len(mix) < target_samples:
            mix = np.concatenate(
                [mix, np.zeros((target_samples - len(mix), 2))], axis=0
            )
        elif len(mix) > target_samples:
            mix = mix[:target_samples]

        # Set the final integrated loudness relative to the voice reference.
        measured = measure_lufs_from_array(mix)
        gain = 10.0 ** ((target_bg_lufs - measured) / 20.0)
        mix = np.clip(mix * gain, -1.0, 1.0)
        final_lufs = measure_lufs_from_array(mix)

        wav = out_dir / "igem-presentation-background-15min.wav"
        mp3 = out_dir / "igem-presentation-background-15min.mp3"
        sf.write(str(wav), mix, SR, subtype="PCM_16")
        encode_mp3(wav, mp3)

        # A short level-check mix with the actual voice test.  It is not the
        # full presentation, just a way to audition the voice/music balance.
        voice, voice_sr = sf.read(str(voice_wav), always_2d=True, dtype="float64")
        if voice_sr != SR:
            raise MixError(f"unexpected voice sample rate: {voice_sr}")
        n = min(len(voice), len(mix))
        preview = np.clip(voice[:n] + mix[:n], -1.0, 1.0)
        preview_wav = out_dir / "presentation-level-test.wav"
        preview_mp3 = out_dir / "presentation-level-test.mp3"
        sf.write(str(preview_wav), preview, SR, subtype="PCM_16")
        encode_mp3(preview_wav, preview_mp3)

        info = {
            "title": config.get("title", "iGEM presentation background"),
            "voice_reference": str(voice_mp4),
            "voice_lufs": round(voice_lufs, 3),
            "voice_to_background_offset_db": round(
                voice_lufs - target_bg_lufs, 3
            ),
            "target_background_lufs": round(target_bg_lufs, 3),
            "measured_background_lufs": round(final_lufs, 3),
            "total_duration_s": round(len(mix) / SR, 3),
            "section_gap_s": gap,
            "loop_seam_fade_ms": seam_ms,
            "wav": str(wav),
            "mp3": str(mp3),
            "level_test_mp3": str(preview_mp3),
            "sections": cues,
        }
        (out_dir / "presentation-background-cues.json").write_text(
            json.dumps(info, ensure_ascii=False, indent=2)
        )
        return info


def measure_lufs_from_array(audio: np.ndarray) -> float:
    """Measure an in-memory mix by writing a temporary WAV."""
    with tempfile.NamedTemporaryFile(suffix=".wav", delete=False) as handle:
        path = Path(handle.name)
    try:
        sf.write(str(path), np.clip(audio, -1.0, 1.0), SR, subtype="PCM_24")
        return measure_lufs(path)
    finally:
        path.unlink(missing_ok=True)


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Render the strict-loop presentation background"
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
