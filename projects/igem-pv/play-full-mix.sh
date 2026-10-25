#!/usr/bin/env bash
# Play the full 15-minute presentation mix.
set -euo pipefail

FILE="/home/cjc/Downloads/iGEM 配乐/presentation/presentation-voice-loop-with-background-15min.mp3"
LOG="/tmp/opencode/play-full-mix.log"

if [ ! -f "$FILE" ]; then
  echo "missing mix: $FILE" >&2
  echo "run render-presentation.sh first" >&2
  exit 1
fi

if pgrep -x ffplay >/dev/null 2>&1; then
  echo "an ffplay process is already running; stop it first" >&2
  exit 1
fi

: > "$LOG"
{
  echo "PLAYLIST_START $(date -Is)"
  echo "PLAY full 15min background + voice $(date +%H:%M:%S)"
  ffplay -nodisp -autoexit -hide_banner -loglevel error -volume 85 "$FILE"
  echo "STOP full mix rc=$? $(date +%H:%M:%S)"
  echo "PLAYLIST_END $(date -Is)"
} >> "$LOG" 2>&1
