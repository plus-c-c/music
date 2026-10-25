#!/usr/bin/env python3
"""Extract isolated presentation parts for comparison.

Currently this extracts the hardware background section and the voice
reference as separate files, then can play them sequentially.
"""

from __future__ import annotations

import argparse
import json
import math
import os
import shlex
import shutil
import subprocess
import sys
from pathlib import Path

import numpy as np

HERE = Path(__file__).resolve().parent
DEFAULT_AUDIO_DIR = Path(
    os.environ.get("IGEM_AUDIO_DIR", "/home/cjc/Downloads/iGEM 配乐")
)
DEFAULT_BACKGROUND = (
    DEFAULT_AUDIO_DIR
    / "presentation"
    / "presentation-background-plus10db.wav"
)
DEFAULT_VOICE = DEFAULT_AUDIO_DIR / "presentation" / "test-sound.wav"
DEFAULT_CUES = (
    DEFAULT_AUDIO_DIR / "presentation" / "presentation-background-cues.json"
)
DEFAULT_OUT_DIR = DEFAULT_AUDIO_DIR / "presentation"


class IsolateError(RuntimeError):
    pass


def require(name: str) -> None:
    if not shutil.which(name):
        raise IsolateError(f"required executable not found: {name}")


def extract_segment(src: Path, dst_wav: Path, start: float, duration: float) -> None:
    require("ffmpeg")
    p = subprocess.run(
        [
            "ffmpeg",
            "-y",
            "-hide_banner",
            "-loglevel",
            "error",
            "-ss",
            f"{start:.6f}",
            "-t",
            f"{duration:.6f}",
            "-i",
            str(src),
            "-ar",
            "48000",
            "-ac",
            "2",
            "-c:a",
            "pcm_s16le",
            str(dst_wav),
        ],
        capture_output=True,
        text=True,
    )
    if p.returncode != 0:
        raise IsolateError(f"ffmpeg extraction failed:\n{p.stderr[-2000:]}")


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
        raise IsolateError(f"MP3 encode failed:\n{p.stderr[-2000:]}")


def peak_dbfs(path: Path) -> float:
    import soundfile as sf

    data, _ = sf.read(str(path), always_2d=True, dtype="float32")
    peak = float(np.max(np.abs(data))) if len(data) else 0.0
    return 20.0 * math.log10(peak + 1e-12)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--background", default=str(DEFAULT_BACKGROUND))
    parser.add_argument("--voice", default=str(DEFAULT_VOICE))
    parser.add_argument("--cues", default=str(DEFAULT_CUES))
    parser.add_argument("--out-dir", default=str(DEFAULT_OUT_DIR))
    parser.add_argument("--section", default="硬件")
    parser.add_argument("--play", action="store_true")
    args = parser.parse_args()
    require("ffmpeg")
    bg = Path(args.background)
    voice = Path(args.voice)
    cues_path = Path(args.cues)
    out_dir = Path(args.out_dir)
    if not bg.is_file():
        raise IsolateError(f"missing background: {bg}")
    if not voice.is_file():
        raise IsolateError(f"missing voice: {voice}")
    cues = json.loads(cues_path.read_text())["sections"]
    cue = next((s for s in cues if s["name"] == args.section), None)
    if cue is None:
        raise IsolateError(f"section not found: {args.section}")
    out_dir.mkdir(parents=True, exist_ok=True)

    hw_wav = out_dir / "presentation-hardware-background-only.wav"
    hw_mp3 = out_dir / "presentation-hardware-background-only.mp3"
    voice_wav = out_dir / "presentation-voice-reference.wav"
    voice_mp3 = out_dir / "presentation-voice-reference.mp3"
    extract_segment(
        bg,
        hw_wav,
        float(cue["music_start_s"]),
        float(cue["music_duration_s"]),
    )
    encode_mp3(hw_wav, hw_mp3)
    extract_segment(voice, voice_wav, 0.0, 1e9)
    encode_mp3(voice_wav, voice_mp3)
    info = {
        "section": cue["name"],
        "section_start_s": cue["music_start_s"],
        "section_end_s": cue["music_end_s"],
        "section_duration_s": cue["music_duration_s"],
        "hardware_background_mp3": str(hw_mp3),
        "hardware_background_peak_dbfs": round(peak_dbfs(hw_wav), 3),
        "voice_reference_mp3": str(voice_mp3),
        "voice_reference_peak_dbfs": round(peak_dbfs(voice_wav), 3),
    }
    (out_dir / "presentation-isolated-parts.json").write_text(
        json.dumps(info, ensure_ascii=False, indent=2)
    )
    print(json.dumps(info, ensure_ascii=False, indent=2))

    if args.play:
        log = out_dir / "play-isolated-parts.log"
        script = out_dir / "play-isolated-parts.sh"
        lines = [
            "#!/usr/bin/env bash",
            "set -u",
            f"LOG={shlex.quote(str(log))}",
            "{",
            'echo "PLAYLIST_START $(date -Is)"',
        ]
        for label, path in [
            ("hardware background", hw_mp3),
            ("voice reference", voice_mp3),
        ]:
            lines.append(
                f'printf "PLAY %s %s\\n" "{label}" "$(date +%H:%M:%S)"'
            )
            lines.append(
                "ffplay -nodisp -autoexit -hide_banner -loglevel error "
                f"-volume 85 {shlex.quote(str(path))}"
            )
            lines.append(
                f'printf "STOP %s rc=%s %s\\n" "{label}" "$?" "$(date +%H:%M:%S)"'
            )
            lines.append("sleep 1")
        lines += ['echo "PLAYLIST_END $(date -Is)"', "} >> \"$LOG\" 2>&1"]
        script.write_text("\n".join(lines) + "\n")
        script.chmod(0o755)
        subprocess.Popen(
            ["setsid", "bash", str(script)],
            stdin=subprocess.DEVNULL,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            start_new_session=True,
        )
        print(f"started {script}; log: {log}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except IsolateError as exc:
        print(f"error: {exc}", file=sys.stderr)
        raise SystemExit(2)
