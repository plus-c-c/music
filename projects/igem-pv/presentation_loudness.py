#!/usr/bin/env python3
"""Calculate and present the fixed presentation loudness table.

The table always contains:
    段落 | 起始段 | 循环 | 结束段 | 音乐长度 | 段后空隙 |
    人声 LUFS | 背景 LUFS | 差值
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
from pathlib import Path

HERE = Path(__file__).resolve().parent
DEFAULT_AUDIO_DIR = Path(
    os.environ.get("IGEM_AUDIO_DIR", "/home/cjc/Downloads/iGEM 配乐")
)
DEFAULT_VOICE = DEFAULT_AUDIO_DIR / "presentation" / "presentation-voice-loop-15min.wav"
DEFAULT_BACKGROUND = (
    DEFAULT_AUDIO_DIR / "presentation" / "presentation-background-plus10db.wav"
)
DEFAULT_CUES = (
    DEFAULT_AUDIO_DIR / "presentation" / "presentation-background-cues.json"
)
DEFAULT_OUT_DIR = DEFAULT_AUDIO_DIR / "presentation"


class ReportError(RuntimeError):
    pass


def require(name: str) -> None:
    if not shutil.which(name):
        raise ReportError(f"required executable not found: {name}")


def measure_lufs(path: Path, start: float | None = None, duration: float | None = None) -> float:
    require("ffmpeg")
    cmd = ["ffmpeg", "-hide_banner", "-nostats"]
    if start is not None:
        cmd += ["-ss", f"{float(start):.6f}"]
    if duration is not None:
        cmd += ["-t", f"{float(duration):.6f}"]
    cmd += [
        "-i",
        str(path),
        "-filter_complex",
        "ebur128=peak=true",
        "-f",
        "null",
        "-",
    ]
    p = subprocess.run(cmd, capture_output=True, text=True)
    if p.returncode != 0:
        raise ReportError(f"LUFS measurement failed for {path}")
    summary = p.stderr.rsplit("Summary:", 1)[-1]
    m = re.search(
        r"Integrated loudness:\s*\n\s*I:\s*(-?[0-9.]+)\s*LUFS",
        summary,
    )
    if not m:
        raise ReportError(f"could not parse LUFS for {path}")
    return float(m.group(1))


def fmt_range(start: float, end: float) -> str:
    return f"{start:.3f}–{end:.3f}s"


def fmt_loop(cue: dict) -> str:
    return f"{fmt_range(cue['loop_start_s'], cue['loop_end_s'])} ×{cue['loop_repeats']}"


def make_row(
    cue: dict,
    voice: Path,
    background: Path,
) -> dict[str, object]:
    start = float(cue["music_start_s"])
    duration = float(cue["music_duration_s"])
    voice_lufs = measure_lufs(voice, start, duration)
    bg_lufs = measure_lufs(background, start, duration)
    return {
        "name": cue["name"],
        "intro": fmt_range(
            float(cue.get("intro_start_s", 0.0)),
            float(cue.get("intro_end_s", cue["loop_start_s"])),
        ),
        "loop": fmt_loop(cue),
        "outro": fmt_range(
            float(cue.get("outro_start_s", cue["loop_end_s"])),
            float(cue.get("outro_end_s", cue["music_end_s"])),
        ),
        "music_length_s": round(duration, 3),
        "gap_after_s": round(float(cue.get("gap_after_s", 0.0)), 3),
        "voice_lufs": round(voice_lufs, 2),
        "background_lufs": round(bg_lufs, 2),
        "difference_lu": round(voice_lufs - bg_lufs, 2),
    }


def markdown_table(rows: list[dict[str, object]]) -> str:
    lines = [
        "| 段落 | 起始段 | 循环 | 结束段 | 音乐长度 | 段后空隙 | 人声 LUFS | 背景 LUFS | 差值 |",
        "|---|---|---|---|---:|---:|---:|---:|---:|",
    ]
    for r in rows:
        lines.append(
            "| {name} | {intro} | {loop} | {outro} | {music_length_s:.3f}s | "
            "{gap_after_s:.3f}s | {voice_lufs:.2f} | "
            "{background_lufs:.2f} | {difference_lu:.2f} LU |".format(**r)
        )
    return "\n".join(lines)


def weighted_lufs(rows: list[dict[str, object]], key: str) -> float:
    total_duration = sum(float(r["music_length_s"]) for r in rows)
    power = sum(
        float(r["music_length_s"]) * 10.0 ** (float(r[key]) / 10.0)
        for r in rows
    )
    return 10.0 * math.log10(power / total_duration)


def build_report(voice: Path, background: Path, cues_path: Path) -> dict:
    cues = json.loads(cues_path.read_text())["sections"]
    rows = [make_row(c, voice, background) for c in cues]
    v_weighted = weighted_lufs(rows, "voice_lufs")
    b_weighted = weighted_lufs(rows, "background_lufs")
    overall_voice = measure_lufs(voice)
    overall_bg = measure_lufs(background)
    return {
        "columns": [
            "段落",
            "起始段",
            "循环",
            "结束段",
            "音乐长度",
            "段后空隙",
            "人声 LUFS",
            "背景 LUFS",
            "差值",
        ],
        "rows": rows,
        "weighted": {
            "voice_lufs": round(v_weighted, 2),
            "background_lufs": round(b_weighted, 2),
            "difference_lu": round(v_weighted - b_weighted, 2),
        },
        "overall": {
            "voice_lufs": round(overall_voice, 2),
            "background_lufs": round(overall_bg, 2),
            "difference_lu": round(overall_voice - overall_bg, 2),
        },
    }


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Calculate the fixed per-section voice/background loudness table"
    )
    parser.add_argument("--voice", default=str(DEFAULT_VOICE))
    parser.add_argument("--background", default=str(DEFAULT_BACKGROUND))
    parser.add_argument("--cues", default=str(DEFAULT_CUES))
    parser.add_argument("--out-dir", default=str(DEFAULT_OUT_DIR))
    parser.add_argument("--no-write", action="store_true")
    args = parser.parse_args()
    voice = Path(args.voice)
    background = Path(args.background)
    cues = Path(args.cues)
    for p in (voice, background, cues):
        if not p.is_file():
            raise ReportError(f"missing input: {p}")
    report = build_report(voice, background, cues)
    md = markdown_table(report["rows"])
    overall = report["overall"]
    weighted = report["weighted"]
    print(md)
    print()
    print(
        f"整体：人声 {overall['voice_lufs']:.2f} LUFS，"
        f"背景 {overall['background_lufs']:.2f} LUFS，"
        f"差值 {overall['difference_lu']:.2f} LU"
    )
    print(
        f"分段时长加权：人声 {weighted['voice_lufs']:.2f} LUFS，"
        f"背景 {weighted['background_lufs']:.2f} LUFS，"
        f"差值 {weighted['difference_lu']:.2f} LU"
    )
    if not args.no_write:
        out_dir = Path(args.out_dir)
        out_dir.mkdir(parents=True, exist_ok=True)
        (out_dir / "presentation-loudness-report.json").write_text(
            json.dumps(report, ensure_ascii=False, indent=2)
        )
        (out_dir / "presentation-loudness-report.md").write_text(
            "# Presentation loudness report\n\n"
            + md
            + "\n\n"
            + f"- 整体：人声 {overall['voice_lufs']:.2f} LUFS，"
            f"背景 {overall['background_lufs']:.2f} LUFS，"
            f"差值 {overall['difference_lu']:.2f} LU\n"
            + f"- 分段时长加权：人声 {weighted['voice_lufs']:.2f} LUFS，"
            f"背景 {weighted['background_lufs']:.2f} LUFS，"
            f"差值 {weighted['difference_lu']:.2f} LU\n",
            encoding="utf-8",
        )
        print(f"wrote {out_dir / 'presentation-loudness-report.md'}")
        print(f"wrote {out_dir / 'presentation-loudness-report.json'}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except ReportError as exc:
        print(f"error: {exc}", file=sys.stderr)
        raise SystemExit(2)
