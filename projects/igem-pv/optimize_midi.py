#!/usr/bin/env python3
"""Clean up Aria-AMT / performance MIDI for editing and notation.

The processor intentionally does not try to invent missing notes.  It removes
low-confidence events, optionally quantizes note starts to a beat grid,
normalizes tempo metadata, trims overlapping same-pitch notes, and cleans
sustain-pedal events.
"""

from __future__ import annotations

import argparse
import json
import math
import sys
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path

import mido

DEFAULT_MIDI_DIR = Path(
    "/home/cjc/Downloads/iGEM 配乐/aria-amt-midi"
)
DEFAULT_TRACKS = ["开头", "NEST开始后", "2", "3", "结尾"]


class OptimizeError(RuntimeError):
    pass


@dataclass
class Note:
    start: int
    end: int
    pitch: int
    velocity: int
    channel: int


@dataclass
class ParsedMidi:
    ticks_per_beat: int
    bpm: float
    notes: list[Note]
    messages: list[tuple[int, int, mido.Message]]
    max_tick: int


def parse_midi(path: Path) -> ParsedMidi:
    mid = mido.MidiFile(path)
    tpb = mid.ticks_per_beat
    bpm = 120.0
    notes: list[Note] = []
    active: dict[tuple[int, int], list[tuple[int, int]]] = defaultdict(list)
    messages: list[tuple[int, int, mido.Message]] = []
    tick = 0
    order = 0

    for msg in mido.merge_tracks(mid.tracks):
        tick += msg.time
        order += 1
        if msg.type == "set_tempo" and bpm == 120.0:
            bpm = 60_000_000.0 / msg.tempo
        if msg.type == "note_on" and msg.velocity > 0:
            active[(msg.channel, msg.note)].append((tick, msg.velocity))
        elif msg.type in {"note_off"} or (
            msg.type == "note_on" and msg.velocity == 0
        ):
            key = (msg.channel, msg.note)
            if active[key]:
                start, velocity = active[key].pop(0)
                notes.append(
                    Note(start, tick, msg.note, velocity, msg.channel)
                )
        elif msg.type == "end_of_track":
            continue
        elif msg.type == "set_tempo":
            # Aria emits a single fixed tempo.  Keep it out of the preserved
            # stream; the output writer creates the final tempo event.
            continue
        elif msg.type == "note_on":
            continue
        else:
            messages.append((tick, order, msg.copy(time=0)))

    max_tick = tick
    for key, pending in active.items():
        for start, velocity in pending:
            notes.append(
                Note(start, max_tick, key[1], velocity, key[0])
            )
    return ParsedMidi(tpb, bpm, notes, messages, max_tick)


def quantize(value: int, grid: int) -> int:
    return int(round(value / grid)) * grid


def clean_notes(
    parsed: ParsedMidi,
    min_velocity: int,
    min_duration_ms: float,
    bpm: float,
    trim_overlaps: bool,
) -> tuple[list[Note], dict[str, int]]:
    min_ticks = int(
        round(min_duration_ms * bpm * parsed.ticks_per_beat / 60_000.0)
    )
    stats = {
        "input_notes": len(parsed.notes),
        "low_velocity": 0,
        "short": 0,
        "overlap_trimmed": 0,
    }
    notes = []
    for note in parsed.notes:
        if note.velocity < min_velocity:
            stats["low_velocity"] += 1
            continue
        if note.end - note.start < min_ticks:
            stats["short"] += 1
            continue
        notes.append(note)

    if not trim_overlaps:
        return notes, stats

    grouped: dict[tuple[int, int], list[Note]] = defaultdict(list)
    for note in notes:
        grouped[(note.channel, note.pitch)].append(note)

    cleaned: list[Note] = []
    for (_, _), group in grouped.items():
        group.sort(key=lambda n: (n.start, n.end, -n.velocity))
        kept: list[Note] = []
        for note in group:
            if not kept:
                kept.append(note)
                continue
            prev = kept[-1]
            if note.start < prev.end:
                # Same onset: keep the stronger note, discard the duplicate.
                if note.start == prev.start:
                    if note.velocity > prev.velocity:
                        kept[-1] = note
                    stats["overlap_trimmed"] += 1
                    continue
                # Retrigger: shorten the previous note to the new onset.
                prev.end = min(prev.end, note.start)
                if prev.end - prev.start < min_ticks:
                    kept.pop()
                stats["overlap_trimmed"] += 1
            kept.append(note)
        cleaned.extend(kept)

    cleaned.sort(key=lambda n: (n.start, n.end, n.pitch))
    return cleaned, stats


def clean_pedals(
    parsed: ParsedMidi,
    notes: list[Note],
) -> tuple[list[tuple[int, int, mido.Message]], dict[str, int]]:
    pedals = [
        (tick, order, msg)
        for tick, order, msg in parsed.messages
        if msg.type == "control_change" and msg.control == 64
    ]
    others = [
        (tick, order, msg)
        for tick, order, msg in parsed.messages
        if not (msg.type == "control_change" and msg.control == 64)
    ]
    pedals.sort(key=lambda x: (x[0], x[1]))
    cleaned: list[tuple[int, int, mido.Message]] = []
    last_value: int | None = None
    removed = 0
    for tick, order, msg in pedals:
        if msg.value == last_value:
            removed += 1
            continue
        cleaned.append((tick, order, msg))
        last_value = msg.value
    # If the file leaves the pedal down, close it at the final note end.
    if cleaned and cleaned[-1][2].value > 0:
        if notes:
            end_tick = max(n.end for n in notes)
            cleaned.append(
                (
                    end_tick,
                    10_000,
                    mido.Message(
                        "control_change",
                        channel=cleaned[-1][2].channel,
                        control=64,
                        value=0,
                        time=0,
                    ),
                )
            )
    return others + cleaned, {"pedal_removed": removed}


def build_cleaned_midi(
    parsed: ParsedMidi,
    notes: list[Note],
    messages: list[tuple[int, int, mido.Message]],
    bpm: float,
    quantize_grid: int | None,
    source_bpm: float,
) -> mido.MidiFile:
    tpb = parsed.ticks_per_beat
    scale = bpm / source_bpm

    def scale_tick(tick: int) -> int:
        return int(round(tick * scale))

    events: list[tuple[int, int, mido.Message]] = []
    final_tick = 0
    for note in notes:
        start = scale_tick(note.start)
        end = scale_tick(note.end)
        duration = max(1, end - start)
        if quantize_grid:
            start = quantize(start, quantize_grid)
        end = start + duration
        final_tick = max(final_tick, end)
        events.append(
            (
                start,
                2,
                mido.Message(
                    "note_on",
                    channel=note.channel,
                    note=note.pitch,
                    velocity=note.velocity,
                    time=0,
                ),
            )
        )
        events.append(
            (
                end,
                1,
                mido.Message(
                    "note_off",
                    channel=note.channel,
                    note=note.pitch,
                    velocity=0,
                    time=0,
                ),
            )
        )

    for tick, order, msg in messages:
        new_tick = scale_tick(tick)
        if quantize_grid and msg.type == "control_change":
            new_tick = quantize(new_tick, quantize_grid)
        final_tick = max(final_tick, new_tick)
        events.append((new_tick, order + 10, msg))

    events.sort(key=lambda x: (x[0], x[1]))
    mid = mido.MidiFile(type=0, ticks_per_beat=tpb)
    track = mido.MidiTrack()
    mid.tracks.append(track)
    track.append(
        mido.MetaMessage(
            "set_tempo",
            tempo=int(round(60_000_000.0 / bpm)),
            time=0,
        )
    )
    prev = 0
    for tick, _, msg in events:
        track.append(msg.copy(time=max(0, tick - prev)))
        prev = tick
    track.append(
        mido.MetaMessage("end_of_track", time=max(0, final_tick - prev))
    )
    return mid


def optimize_file(
    src: Path,
    dst: Path,
    args: argparse.Namespace,
) -> dict[str, object]:
    parsed = parse_midi(src)
    target_bpm = float(args.bpm) if args.bpm is not None else parsed.bpm
    notes, stats = clean_notes(
        parsed,
        min_velocity=int(args.min_velocity),
        min_duration_ms=float(args.min_duration_ms),
        bpm=target_bpm,
        trim_overlaps=bool(args.trim_overlaps),
    )
    messages, pedal_stats = clean_pedals(parsed, notes)
    if args.quantize == "none":
        grid = None
    else:
        denominator = 4 if args.quantize == "1/16" else 8
        if parsed.ticks_per_beat % denominator:
            raise OptimizeError(
                f"ticks_per_beat={parsed.ticks_per_beat} cannot represent {args.quantize}"
            )
        grid = parsed.ticks_per_beat // denominator

    mid = build_cleaned_midi(
        parsed,
        notes,
        messages,
        target_bpm,
        grid,
        parsed.bpm,
    )
    dst.parent.mkdir(parents=True, exist_ok=True)
    mid.save(dst)
    stats.update(pedal_stats)
    stats.update(
        {
            "output_notes": len(notes),
            "source_bpm": round(parsed.bpm, 3),
            "output_bpm": round(target_bpm, 3),
            "quantize": args.quantize,
            "output": str(dst),
        }
    )
    return stats


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Clean and quantize performance MIDI without changing pitch"
    )
    parser.add_argument("--midi-dir", default=str(DEFAULT_MIDI_DIR))
    parser.add_argument("--out-dir", default=None)
    parser.add_argument("--tracks", default=None)
    parser.add_argument(
        "--bpm",
        type=float,
        default=None,
        help="target constant BPM; omit to keep the source tempo metadata",
    )
    parser.add_argument(
        "--quantize", choices=["none", "1/16", "1/32"], default="1/16"
    )
    parser.add_argument("--min-velocity", type=int, default=10)
    parser.add_argument("--min-duration-ms", type=float, default=30.0)
    parser.add_argument(
        "--trim-overlaps",
        action=argparse.BooleanOptionalAction,
        default=True,
    )
    parser.add_argument("--prefix", default="optimized-")
    args = parser.parse_args()

    midi_dir = Path(args.midi_dir)
    if not midi_dir.is_dir():
        raise OptimizeError(f"MIDI directory not found: {midi_dir}")
    out_dir = Path(args.out_dir) if args.out_dir else midi_dir.parent / "aria-amt-midi-optimized"
    if args.tracks:
        names = [x.strip() for x in args.tracks.split(",") if x.strip()]
        paths = [midi_dir / f"{n}.mid" for n in names]
    else:
        paths = sorted(midi_dir.glob("*.mid"))

    summaries = []
    for src in paths:
        if not src.is_file():
            print(f"skip missing: {src}", file=sys.stderr)
            continue
        dst = out_dir / f"{args.prefix}{src.name}"
        info = optimize_file(src, dst, args)
        summaries.append(info)
        print(
            f"{src.name}: {info['input_notes']} -> {info['output_notes']} notes, "
            f"low_vel={info['low_velocity']}, short={info['short']}, "
            f"overlap={info['overlap_trimmed']}, pedal={info['pedal_removed']}"
        )
    (out_dir / "optimize-report.json").write_text(
        json.dumps(summaries, ensure_ascii=False, indent=2)
    )
    print(f"wrote {out_dir / 'optimize-report.json'}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except OptimizeError as exc:
        print(f"error: {exc}", file=sys.stderr)
        raise SystemExit(2)
