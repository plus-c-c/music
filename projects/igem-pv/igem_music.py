#!/usr/bin/env python3
"""Reproducible iGEM music workflow.

Subcommands
-----------
normalize
    Measure the sample peak of each source MP3 and of each MIDI rendered by
    FluidSynth.  Write the per-file gains needed for a common peak level.

detect
    Find beat-aligned loop candidates in the middle of each MP3.  The scoring
    uses harmonic similarity at the loop seam, energy continuity, boundary
    onset strength, and distance from the middle of the track.

render
    Render the selected loop candidates as
        intro + loop + loop + outro
    using an equal-power crossfade only at the loop seam.

play
    Play loop previews, normalized MIDI/MP3 pairs, or the original MP3s.

The default paths match the current iGEM working set.  All paths are also
available as command-line options or IGEM_* environment variables.
"""

from __future__ import annotations

import argparse
import json
import math
import os
import re
import shlex
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Iterable, Sequence

import librosa
import numpy as np
import soundfile as sf

HERE = Path(__file__).resolve().parent
DEFAULT_AUDIO_DIR = Path(
    os.environ.get("IGEM_AUDIO_DIR", "/home/cjc/Downloads/iGEM 配乐")
)
DEFAULT_MIDI_DIR = Path(
    os.environ.get("IGEM_MIDI_DIR", str(DEFAULT_AUDIO_DIR / "aria-amt-midi"))
)
DEFAULT_WORK_DIR = Path(
    os.environ.get("IGEM_WORK_DIR", str(DEFAULT_AUDIO_DIR / "loop-preview"))
)
DEFAULT_SF2 = Path(
    os.environ.get("IGEM_SF2", "/usr/share/soundfonts/FluidR3_GM.sf2")
)
DEFAULT_SELECTION = HERE / "presets" / "igem-2026.json"
DEFAULT_TRACKS = ["开头", "NEST开始后", "2", "3", "结尾"]
DEFAULT_TARGET_DBFS = -2.0


class WorkflowError(RuntimeError):
    pass


def log(*args: object) -> None:
    print(*args, flush=True)


def require_executable(name: str) -> str:
    path = shutil.which(name)
    if not path:
        raise WorkflowError(f"required executable not found: {name}")
    return path


def audio_duration(path: Path) -> float:
    if shutil.which("ffprobe"):
        p = subprocess.run(
            [
                "ffprobe",
                "-v",
                "error",
                "-show_entries",
                "format=duration",
                "-of",
                "default=nw=1:nk=1",
                str(path),
            ],
            capture_output=True,
            text=True,
        )
        if p.returncode == 0:
            return float(p.stdout.strip())
    data, sr = sf.read(str(path), always_2d=True)
    return len(data) / sr


def measure_peak_db(path: Path, volume: float | None = None) -> float:
    """Return FFmpeg's sample peak in dBFS."""
    require_executable("ffmpeg")
    filters = []
    if volume is not None:
        filters.append(f"volume={volume:.12f}:precision=double")
    filters.append("volumedetect")
    p = subprocess.run(
        [
            "ffmpeg",
            "-hide_banner",
            "-nostdin",
            "-i",
            str(path),
            "-af",
            ",".join(filters),
            "-f",
            "null",
            "-",
        ],
        capture_output=True,
        text=True,
    )
    if p.returncode != 0:
        raise WorkflowError(f"ffmpeg failed for {path}:\n{p.stderr[-2000:]}")
    m = re.search(r"max_volume:\s*(-?inf|-?[0-9.]+)\s*dB", p.stderr)
    if not m:
        raise WorkflowError(f"could not read max_volume for {path}")
    value = m.group(1)
    if value == "-inf":
        return float("-inf")
    return float(value)


def db_to_gain(db: float) -> float:
    return 10.0 ** (db / 20.0)


def gain_to_db(gain: float) -> float:
    return 20.0 * math.log10(max(gain, 1e-12))


def track_names(args: argparse.Namespace) -> list[str]:
    if getattr(args, "tracks", None):
        return [x.strip() for x in args.tracks.split(",") if x.strip()]
    return list(DEFAULT_TRACKS)


def decode_mp3(path: Path, out: Path, sr: int = 48000) -> None:
    require_executable("ffmpeg")
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
            str(sr),
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
        raise WorkflowError(f"could not decode {path}:\n{p.stderr[-2000:]}")


def render_midi(
    midi: Path,
    out: Path,
    gain: float,
    sf2: Path,
    sf_polyphony: int = 512,
) -> None:
    require_executable("fluidsynth")
    if not sf2.is_file():
        raise WorkflowError(f"SoundFont not found: {sf2}")
    out.parent.mkdir(parents=True, exist_ok=True)
    if out.exists():
        out.unlink()
    cmd = [
        "fluidsynth",
        "-F",
        str(out),
        "-T",
        "wav",
        "-O",
        "s16",
        "-r",
        "48000",
        "-g",
        f"{gain:.8f}",
        "-n",
        "-i",
        "-L",
        "2",
        "-o",
        f"synth.polyphony={sf_polyphony}",
        str(sf2),
        str(midi),
    ]
    p = subprocess.run(cmd, capture_output=True, text=True)
    if p.returncode != 0 or not out.is_file():
        raise WorkflowError(f"fluidsynth failed for {midi}:\n{p.stderr[-2000:]}")


# ---------------------------------------------------------------------------
# normalize
# ---------------------------------------------------------------------------


def cmd_normalize(args: argparse.Namespace) -> int:
    tracks = track_names(args)
    audio_dir = Path(args.audio_dir)
    midi_dir = Path(args.midi_dir)
    work_dir = Path(args.work_dir)
    ref_dir = work_dir / "midi-reference"
    ref_dir.mkdir(parents=True, exist_ok=True)
    sf2 = Path(args.sf2)
    target = float(args.target_dbfs)
    ref_gain = float(args.reference_gain)

    result: dict[str, object] = {
        "target_dbfs": target,
        "reference_midi_gain": ref_gain,
        "tracks": {},
    }
    log(f"target peak: {target:.2f} dBFS")
    log(f"{'track':<14} {'MP3 peak':>10} {'MP3 gain':>10} {'MIDI peak':>10} {'MIDI gain':>10}")
    for name in tracks:
        mp3 = audio_dir / f"{name}.mp3"
        midi = midi_dir / f"{name}.mid"
        if not mp3.is_file():
            raise WorkflowError(f"missing MP3: {mp3}")
        if not midi.is_file():
            raise WorkflowError(f"missing MIDI: {midi}")

        mp3_peak = measure_peak_db(mp3)
        if not math.isfinite(mp3_peak):
            raise WorkflowError(f"silent MP3: {mp3}")
        mp3_gain = db_to_gain(target - mp3_peak)
        # FFmpeg's volume filter can round slightly differently from the
        # analytic gain.  Iterate once or twice until the measured output is
        # actually on target.
        final_mp3_peak = measure_peak_db(mp3, volume=mp3_gain)
        for _ in range(2):
            if abs(final_mp3_peak - target) <= 0.10:
                break
            if not math.isfinite(final_mp3_peak):
                break
            mp3_gain *= db_to_gain(target - final_mp3_peak)
            final_mp3_peak = measure_peak_db(mp3, volume=mp3_gain)
        if abs(final_mp3_peak - target) > 0.25:
            log(
                f"warning: {name}.mp3 final peak is {final_mp3_peak:.2f} dBFS "
                f"(target {target:.2f})"
            )

        ref_wav = ref_dir / f"{name}.wav"
        if args.force or not ref_wav.is_file():
            render_midi(midi, ref_wav, ref_gain, sf2, args.polyphony)
        midi_peak = measure_peak_db(ref_wav)
        if not math.isfinite(midi_peak):
            raise WorkflowError(f"silent MIDI render: {midi}")
        midi_gain = ref_gain * db_to_gain(target - midi_peak)
        if midi_gain > 10.0:
            log(
                f"warning: {name}.mid requires fluid gain {midi_gain:.2f} > 10; "
                "increase the reference gain or render/normalize to WAV"
            )

        result["tracks"][name] = {
            "source_mp3": str(mp3),
            "source_midi": str(midi),
            "mp3_peak_dbfs": round(mp3_peak, 3),
            "mp3_gain": round(mp3_gain, 6),
            "mp3_final_peak_dbfs": round(final_mp3_peak, 3),
            "midi_peak_dbfs_at_reference": round(midi_peak, 3),
            "midi_gain": round(midi_gain, 6),
            "midi_reference_wav": str(ref_wav),
        }
        log(
            f"{name:<14} {mp3_peak:>9.2f}  {mp3_gain:>9.4f}  "
            f"{midi_peak:>9.2f}  {midi_gain:>9.4f}"
        )

    gains_path = Path(args.gains) if args.gains else work_dir / "gains.json"
    gains_path.parent.mkdir(parents=True, exist_ok=True)
    gains_path.write_text(json.dumps(result, ensure_ascii=False, indent=2))
    log(f"wrote {gains_path}")
    return 0


# ---------------------------------------------------------------------------
# loop detection
# ---------------------------------------------------------------------------


def _normalize_vector(x: np.ndarray) -> np.ndarray:
    lo = float(np.min(x))
    hi = float(np.max(x))
    if hi - lo < 1e-12:
        return np.zeros_like(x, dtype=float)
    return (x - lo) / (hi - lo)


def analyze_loop_candidates(
    mp3: Path,
    bars: Sequence[int],
    top: int,
    min_start_frac: float,
    max_start_frac: float,
    max_end_frac: float,
) -> dict[str, object]:
    y, sr = librosa.load(str(mp3), sr=22050, mono=True)
    duration = float(librosa.get_duration(y=y, sr=sr))
    onset_env = librosa.onset.onset_strength(y=y, sr=sr, hop_length=512)
    tempo, beats = librosa.beat.beat_track(
        onset_envelope=onset_env,
        sr=sr,
        hop_length=512,
        units="time",
        trim=False,
    )
    beats = np.asarray(beats, dtype=float)
    if len(beats) < 16:
        raise WorkflowError(f"not enough detected beats in {mp3}")

    chroma = librosa.feature.chroma_cqt(y=y, sr=sr, hop_length=512)
    chroma_times = librosa.frames_to_time(
        np.arange(chroma.shape[1]), sr=sr, hop_length=512
    )
    chroma_idx = np.clip(
        np.searchsorted(chroma_times, beats), 0, chroma.shape[1] - 1
    )
    chroma_beats = chroma[:, chroma_idx].T
    chroma_beats = chroma_beats / (
        np.linalg.norm(chroma_beats, axis=1, keepdims=True) + 1e-9
    )

    rms = librosa.feature.rms(y=y, frame_length=2048, hop_length=512)[0]
    rms_times = librosa.frames_to_time(
        np.arange(len(rms)), sr=sr, hop_length=512
    )
    rms_beats = np.interp(beats, rms_times, rms)

    onset_times = librosa.frames_to_time(
        np.arange(len(onset_env)), sr=sr, hop_length=512
    )
    onset_beats = np.interp(beats, onset_times, onset_env)

    candidates: list[dict[str, object]] = []
    for n_bars in bars:
        length_beats = int(n_bars) * 4
        if len(beats) <= length_beats + 1:
            continue
        for i in range(0, len(beats) - length_beats - 1):
            j = i + length_beats
            start = float(beats[i])
            end = float(beats[j])
            if start < min_start_frac * duration:
                continue
            if start > max_start_frac * duration:
                continue
            if end > max_end_frac * duration:
                continue
            start_chroma = chroma_beats[i : min(i + 3, j)].mean(axis=0)
            end_chroma = chroma_beats[max(i, j - 3) : j].mean(axis=0)
            seam = 1.0 - float(
                np.dot(start_chroma, end_chroma)
                / (
                    np.linalg.norm(start_chroma) * np.linalg.norm(end_chroma)
                    + 1e-9
                )
            )
            energy0 = float(rms_beats[max(0, i - 1) : i + 2].mean())
            energy1 = float(rms_beats[max(0, j - 2) : j + 1].mean())
            energy_jump = abs(
                math.log((energy1 + 1e-8) / (energy0 + 1e-8))
            )
            onset_end = float(onset_beats[max(0, j - 1) : j + 2].mean())
            onset_start = float(onset_beats[max(0, i - 1) : i + 2].mean())
            onset_ratio = (onset_end + onset_start) / (
                float(onset_beats.mean()) + 1e-9
            )
            middle_penalty = abs(start - 0.45 * duration) / duration
            score = (
                2.2 * seam
                + 0.45 * energy_jump
                + 0.12 * onset_ratio
                + 1.8 * middle_penalty
            )
            candidates.append(
                {
                    "start": round(start, 4),
                    "end": round(end, 4),
                    "bars": n_bars,
                    "score": round(score, 5),
                    "seam": round(seam, 4),
                    "energy_jump": round(energy_jump, 4),
                    "onset_ratio": round(onset_ratio, 3),
                    "start_beat": i,
                    "end_beat": j,
                    "beat_phase": i % 4,
                }
            )

    candidates.sort(key=lambda c: float(c["score"]))
    distinct: list[dict[str, object]] = []
    for c in candidates:
        if any(abs(float(c["start"]) - float(x["start"])) < 0.5 for x in distinct):
            continue
        distinct.append(c)
        if len(distinct) >= top:
            break

    return {
        "duration": round(duration, 4),
        "tempo_bpm": round(float(np.atleast_1d(tempo)[0]), 3),
        "beats": int(len(beats)),
        "candidates": distinct,
    }


def cmd_detect(args: argparse.Namespace) -> int:
    audio_dir = Path(args.audio_dir)
    bars = [int(x) for x in args.bars.split(",") if x.strip()]
    out: dict[str, object] = {
        "audio_dir": str(audio_dir),
        "candidate_bars": bars,
        "tracks": {},
    }
    for name in track_names(args):
        mp3 = audio_dir / f"{name}.mp3"
        if not mp3.is_file():
            raise WorkflowError(f"missing MP3: {mp3}")
        log(f"analyzing {name} ...")
        info = analyze_loop_candidates(
            mp3,
            bars=bars,
            top=args.top,
            min_start_frac=args.min_start_frac,
            max_start_frac=args.max_start_frac,
            max_end_frac=args.max_end_frac,
        )
        out["tracks"][name] = info
        log(
            f"  {info['duration']:.3f}s, {info['tempo_bpm']:.2f} BPM, "
            f"{info['beats']} beats"
        )
        for c in info["candidates"]:
            log(
                f"  {c['start']:8.3f}-{c['end']:8.3f} "
                f"{c['bars']} bars  seam={c['seam']:.3f} "
                f"energy={c['energy_jump']:.3f} "
                f"onset={c['onset_ratio']:.2f}"
            )
    out_path = Path(args.out) if args.out else Path(args.work_dir) / "candidates.json"
    out_path.parent.mkdir(parents=True, exist_ok=True)
    out_path.write_text(json.dumps(out, ensure_ascii=False, indent=2))
    log(f"wrote {out_path}")
    return 0


# ---------------------------------------------------------------------------
# loop rendering
# ---------------------------------------------------------------------------


def _equal_power_crossfade(a: np.ndarray, b: np.ndarray) -> np.ndarray:
    """Equal-power crossfade from a (fade out) to b (fade in)."""
    n = min(len(a), len(b))
    if n <= 0:
        return np.empty((0, a.shape[1] if a.ndim == 2 else 1))
    t = np.linspace(0.0, np.pi / 2.0, n, endpoint=False)
    w1 = np.cos(t)[:, None]
    w2 = np.sin(t)[:, None]
    return a[-n:] * w1 + b[:n] * w2


def render_loop_preview(
    mp3: Path,
    out_path: Path,
    start_s: float,
    end_s: float,
    fade_ms: float,
    target_dbfs: float,
    sr: int = 48000,
) -> dict[str, object]:
    with tempfile.TemporaryDirectory(
        prefix="igem-loop-", dir=str(Path(out_path).parent)
    ) as td:
        raw = Path(td) / "source.wav"
        decode_mp3(mp3, raw, sr=sr)
        y, actual_sr = sf.read(str(raw), always_2d=True, dtype="float64")
        if actual_sr != sr:
            raise WorkflowError(f"unexpected sample rate {actual_sr} for {mp3}")
        if float(end_s) <= float(start_s):
            raise WorkflowError(
                f"loop end must be after start: {start_s} -> {end_s}"
            )
        s = int(round(float(start_s) * sr))
        e = int(round(float(end_s) * sr))
        s = max(0, min(s, len(y)))
        e = max(s + 1, min(e, len(y)))
        loop = y[s:e]
        intro = y[:s]
        outro = y[e:]
        d = int(round(float(fade_ms) * sr / 1000.0))
        d = max(0, min(d, (e - s) // 2))
        if d:
            seam = _equal_power_crossfade(loop[-d:], loop[:d])
            loop_twice = np.concatenate([loop[:-d], seam, loop[d:]], axis=0)
        else:
            loop_twice = np.concatenate([loop, loop], axis=0)
        result = np.concatenate([intro, loop_twice, outro], axis=0)
        peak = float(np.max(np.abs(result))) if len(result) else 0.0
        target_peak = db_to_gain(target_dbfs)
        gain = target_peak / peak if peak > 0 else 1.0
        result = result * gain
        result = np.clip(result, -1.0, 1.0)
        out_path.parent.mkdir(parents=True, exist_ok=True)
        sf.write(str(out_path), result, sr, subtype="PCM_16")
        final_peak = float(np.max(np.abs(result))) if len(result) else 0.0
        return {
            "source_mp3": str(mp3),
            "output": str(out_path),
            "loop_start_s": round(s / sr, 4),
            "loop_end_s": round(e / sr, 4),
            "loop_length_s": round((e - s) / sr, 4),
            "crossfade_ms": round(d / sr * 1000.0, 1),
            "output_duration_s": round(len(result) / sr, 4),
            "normalization_gain": round(gain, 6),
            "output_peak_dbfs": round(
                20.0 * math.log10(final_peak + 1e-12), 3
            ),
        }


def cmd_render(args: argparse.Namespace) -> int:
    selection_path = Path(args.selection)
    selection = json.loads(selection_path.read_text())
    audio_dir = Path(args.audio_dir)
    out_dir = Path(args.out_dir) if args.out_dir else Path(args.work_dir)
    out_dir.mkdir(parents=True, exist_ok=True)
    target = float(
        args.target_dbfs
        if args.target_dbfs is not None
        else selection.get("target_dbfs", DEFAULT_TARGET_DBFS)
    )
    order = selection.get("track_order") or DEFAULT_TRACKS
    only = set(args.only.split(",")) if args.only else None
    marker_tracks: dict[str, object] = {}
    for name in order:
        if only and name not in only:
            continue
        spec = selection.get("tracks", {}).get(name)
        if not spec:
            raise WorkflowError(f"no loop selection for {name}")
        mp3 = audio_dir / f"{name}.mp3"
        if not mp3.is_file():
            raise WorkflowError(f"missing MP3: {mp3}")
        out_path = out_dir / f"{name}_loop.wav"
        info = render_loop_preview(
            mp3=mp3,
            out_path=out_path,
            start_s=float(spec["start"]),
            end_s=float(spec["end"]),
            fade_ms=float(spec.get("fade_ms", selection.get("crossfade_ms", 150))),
            target_dbfs=target,
        )
        marker_tracks[name] = info
        log(
            f"{name}: {info['loop_start_s']:.3f}-{info['loop_end_s']:.3f}s, "
            f"fade {info['crossfade_ms']:.0f} ms, "
            f"out {info['output_duration_s']:.3f}s, "
            f"peak {info['output_peak_dbfs']:.2f} dBFS"
        )
    markers = {
        "selection": str(selection_path),
        "target_dbfs": target,
        "tracks": marker_tracks,
    }
    (out_dir / "markers.json").write_text(
        json.dumps(markers, ensure_ascii=False, indent=2)
    )
    log(f"wrote {out_dir / 'markers.json'}")
    return 0


# ---------------------------------------------------------------------------
# playback
# ---------------------------------------------------------------------------


def _ffplay(path: Path, volume: float | None = None) -> list[str]:
    cmd = [
        "ffplay",
        "-nodisp",
        "-autoexit",
        "-hide_banner",
        "-loglevel",
        "error",
        "-volume",
        "100",
    ]
    if volume is not None:
        cmd += ["-af", f"volume={volume:.10f}:precision=double"]
    cmd.append(str(path))
    return cmd


def _fluidsynth(midi: Path, gain: float, sf2: Path) -> list[str]:
    return [
        "fluidsynth",
        "-a",
        "pulseaudio",
        "-n",
        "-i",
        "-L",
        "2",
        "-g",
        f"{gain:.8f}",
        "-o",
        "synth.polyphony=512",
        str(sf2),
        str(midi),
    ]


def _play_entries(
    args: argparse.Namespace, gains: dict[str, object] | None
) -> list[tuple[str, list[str]]]:
    names = track_names(args)
    mode = args.mode
    audio_dir = Path(args.audio_dir)
    midi_dir = Path(args.midi_dir)
    loop_dir = Path(args.loop_dir) if args.loop_dir else Path(args.work_dir)
    sf2 = Path(args.sf2)
    entries: list[tuple[str, list[str]]] = []
    for name in names:
        if mode == "loops":
            path = loop_dir / f"{name}_loop.wav"
            if not path.is_file():
                raise WorkflowError(f"missing loop preview: {path}")
            entries.append((f"loop {name}", _ffplay(path)))
        elif mode == "originals":
            path = audio_dir / f"{name}.mp3"
            if not path.is_file():
                raise WorkflowError(f"missing MP3: {path}")
            entries.append((f"mp3 {name}", _ffplay(path)))
        elif mode == "midi":
            path = midi_dir / f"{name}.mid"
            if not path.is_file():
                raise WorkflowError(f"missing MIDI: {path}")
            if not gains:
                raise WorkflowError("--gains is required for MIDI playback")
            gain = float(gains["tracks"][name]["midi_gain"])
            entries.append((f"midi {name}", _fluidsynth(path, gain, sf2)))
        elif mode == "normalized":
            midi = midi_dir / f"{name}.mid"
            mp3 = audio_dir / f"{name}.mp3"
            if not midi.is_file() or not mp3.is_file():
                raise WorkflowError(f"missing MIDI/MP3 for {name}")
            if not gains:
                raise WorkflowError("--gains is required for normalized playback")
            midi_gain = float(gains["tracks"][name]["midi_gain"])
            mp3_gain = float(gains["tracks"][name]["mp3_gain"])
            entries.append((f"midi {name}", _fluidsynth(midi, midi_gain, sf2)))
            entries.append(
                (f"mp3 {name}", _ffplay(mp3, volume=mp3_gain))
            )
        else:
            raise WorkflowError(f"unknown play mode: {mode}")
    return entries


def _write_playlist_script(
    entries: list[tuple[str, list[str]]],
    path: Path,
    log_path: Path,
    gap_s: float,
) -> None:
    lines = [
        "#!/usr/bin/env bash",
        "set -u",
        f"LOG={shlex.quote(str(log_path))}",
        "{",
        'echo "PLAYLIST_START $(date -Is)"',
    ]
    for label, cmd in entries:
        safe_label = shlex.quote(label)
        lines.append(
            f'printf "PLAY %s %s\\n" {safe_label} "$(date +%H:%M:%S)"'
        )
        lines.append(shlex.join(cmd))
        lines.append(
            f'printf "STOP %s rc=%s %s\\n" {safe_label} "$?" "$(date +%H:%M:%S)"'
        )
        if gap_s > 0:
            lines.append(f"sleep {gap_s:g}")
    lines += [
        'echo "PLAYLIST_END $(date -Is)"',
        "} >> \"$LOG\" 2>&1",
    ]
    path.write_text("\n".join(lines) + "\n")
    path.chmod(0o755)


def cmd_play(args: argparse.Namespace) -> int:
    gains: dict[str, object] | None = None
    gains_path = (
        Path(args.gains) if args.gains else Path(args.work_dir) / "gains.json"
    )
    if args.mode in {"midi", "normalized"}:
        if not gains_path.is_file():
            raise WorkflowError(
                f"gains file not found: {gains_path}; run normalize first"
            )
        gains = json.loads(gains_path.read_text())
    entries = _play_entries(args, gains)
    if args.list:
        for label, cmd in entries:
            log(f"{label}: {shlex.join(cmd)}")
        return 0
    work_dir = Path(args.work_dir)
    work_dir.mkdir(parents=True, exist_ok=True)
    script = work_dir / f"play-{args.mode}.sh"
    log_path = Path(args.log) if args.log else work_dir / f"play-{args.mode}.log"
    _write_playlist_script(entries, script, log_path, float(args.gap))
    if args.background:
        proc = subprocess.Popen(
            ["setsid", "bash", str(script)],
            stdin=subprocess.DEVNULL,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            start_new_session=True,
        )
        (work_dir / f"play-{args.mode}.pid").write_text(str(proc.pid) + "\n")
        log(f"started {script} (pid {proc.pid}); log: {log_path}")
    else:
        log(f"running {script}; log: {log_path}")
        subprocess.run(["bash", str(script)], check=False)
    return 0


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------


def add_common_paths(parser: argparse.ArgumentParser) -> None:
    parser.add_argument(
        "--audio-dir",
        default=str(DEFAULT_AUDIO_DIR),
        help=f"source MP3 directory (default: {DEFAULT_AUDIO_DIR})",
    )
    parser.add_argument(
        "--midi-dir",
        default=str(DEFAULT_MIDI_DIR),
        help=f"Aria-AMT MIDI directory (default: {DEFAULT_MIDI_DIR})",
    )
    parser.add_argument(
        "--work-dir",
        default=str(DEFAULT_WORK_DIR),
        help=f"working/output directory (default: {DEFAULT_WORK_DIR})",
    )
    parser.add_argument(
        "--tracks",
        default=",".join(DEFAULT_TRACKS),
        help="comma-separated track names in playback order",
    )


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Reproducible iGEM music normalization, looping, and playback"
    )
    sub = parser.add_subparsers(dest="command", required=True)

    p_norm = sub.add_parser("normalize", help="compute per-file playback gains")
    add_common_paths(p_norm)
    p_norm.add_argument("--target-dbfs", type=float, default=DEFAULT_TARGET_DBFS)
    p_norm.add_argument(
        "--reference-gain",
        type=float,
        default=1.0,
        help="FluidSynth gain used for the measurement render",
    )
    p_norm.add_argument("--sf2", default=str(DEFAULT_SF2))
    p_norm.add_argument("--polyphony", type=int, default=512)
    p_norm.add_argument("--gains", default=None)
    p_norm.add_argument("--force", action="store_true")
    p_norm.set_defaults(func=cmd_normalize)

    p_detect = sub.add_parser("detect", help="find beat-aligned loop candidates")
    add_common_paths(p_detect)
    p_detect.add_argument("--bars", default="4,8")
    p_detect.add_argument("--top", type=int, default=6)
    p_detect.add_argument("--min-start-frac", type=float, default=0.18)
    p_detect.add_argument("--max-start-frac", type=float, default=0.68)
    p_detect.add_argument("--max-end-frac", type=float, default=0.88)
    p_detect.add_argument("--out", default=None)
    p_detect.set_defaults(func=cmd_detect)

    p_render = sub.add_parser("render", help="render start-loop-loop-end previews")
    add_common_paths(p_render)
    p_render.add_argument("--selection", default=str(DEFAULT_SELECTION))
    p_render.add_argument("--out-dir", default=None)
    p_render.add_argument("--only", default=None)
    p_render.add_argument("--target-dbfs", type=float, default=None)
    p_render.set_defaults(func=cmd_render)

    p_play = sub.add_parser("play", help="play previews or source/rendered files")
    add_common_paths(p_play)
    p_play.add_argument(
        "--mode",
        choices=["loops", "normalized", "originals", "midi"],
        default="loops",
    )
    p_play.add_argument("--loop-dir", default=None)
    p_play.add_argument("--gains", default=None)
    p_play.add_argument("--sf2", default=str(DEFAULT_SF2))
    p_play.add_argument("--gap", type=float, default=1.0)
    p_play.add_argument("--background", action="store_true")
    p_play.add_argument("--list", action="store_true")
    p_play.add_argument("--log", default=None)
    p_play.set_defaults(func=cmd_play)
    return parser


def main(argv: Sequence[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    try:
        return int(args.func(args))
    except WorkflowError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 2
    except KeyboardInterrupt:
        print("interrupted", file=sys.stderr)
        return 130


if __name__ == "__main__":
    raise SystemExit(main())
