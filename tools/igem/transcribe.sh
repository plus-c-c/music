#!/usr/bin/env bash
# Transcribe all top-level MP3s with Aria-AMT on the remote iGEM server.
#
# The remote side launches one detached-style background process per track and
# limits concurrency with JOBS.  Each process gets THREADS CPU threads.  The
# local shell waits for all remote workers, then copies the MIDI files back.
#
# Prerequisites on the remote host:
#   - Aria-AMT checkout with aria-amt-cpu.patch applied
#   - Python environment with the dependencies from pyproject.toml
#   - checkpoints/piano-medium-double-1.0.safetensors
#
# Usage:
#   ./transcribe.sh
#   IGEM_THREADS=3 IGEM_JOBS=5 ./transcribe.sh
#   ./transcribe.sh --audio-dir "/path/to/mp3s" --midi-dir "/path/to/midi"
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REMOTE="${IGEM_REMOTE:-igem}"
REMOTE_ROOT="${IGEM_REMOTE_ROOT:-/home/igem/projects/aria-amt}"
REMOTE_PY="${IGEM_REMOTE_PY:-/home/igem/.local/miniconda3/envs/aria-amt/bin/python}"
AUDIO_DIR="${IGEM_AUDIO_DIR:-/home/cjc/Downloads/iGEM 配乐}"
MIDI_DIR="${IGEM_MIDI_DIR:-${AUDIO_DIR}/aria-amt-midi}"
REMOTE_INPUT="${IGEM_REMOTE_INPUT:-${REMOTE_ROOT}/input/iGEM}"
REMOTE_OUTPUT="${IGEM_REMOTE_OUTPUT:-${REMOTE_ROOT}/output/iGEM}"
CHECKPOINT="${IGEM_CHECKPOINT:-checkpoints/piano-medium-double-1.0.safetensors}"
THREADS="${IGEM_THREADS:-3}"
JOBS="${IGEM_JOBS:-5}"
FORCE=0

usage() {
  cat <<EOF
Usage: $(basename "$0") [options]

Options:
  --audio-dir DIR       local MP3 directory (default: $AUDIO_DIR)
  --midi-dir DIR        local MIDI output directory (default: $MIDI_DIR)
  --remote HOST         SSH host (default: $REMOTE)
  --remote-root DIR     remote Aria-AMT checkout (default: $REMOTE_ROOT)
  --remote-python PATH  remote Python executable (default: $REMOTE_PY)
  --threads N           PyTorch threads per worker (default: $THREADS)
  --jobs N              max simultaneous workers (default: $JOBS)
  --checkpoint PATH     checkpoint path relative to remote root
  --force               overwrite existing remote/local MIDI
  -h, --help            show this help

Environment variables with the same names are also supported.
EOF
}

while (($#)); do
  case "$1" in
    --audio-dir) AUDIO_DIR="$2"; shift 2 ;;
    --midi-dir) MIDI_DIR="$2"; shift 2 ;;
    --remote) REMOTE="$2"; shift 2 ;;
    --remote-root) REMOTE_ROOT="$2"; shift 2 ;;
    --remote-python) REMOTE_PY="$2"; shift 2 ;;
    --threads) THREADS="$2"; shift 2 ;;
    --jobs) JOBS="$2"; shift 2 ;;
    --checkpoint) CHECKPOINT="$2"; shift 2 ;;
    --force) FORCE=1; shift ;;
    -h|--help) usage; exit 0 ;;
    *) echo "unknown option: $1" >&2; usage >&2; exit 2 ;;
  esac
done

command -v ssh >/dev/null || { echo "ssh is required" >&2; exit 1; }
command -v scp >/dev/null || { echo "scp is required" >&2; exit 1; }
command -v tar >/dev/null || { echo "tar is required" >&2; exit 1; }
[ -d "$AUDIO_DIR" ] || { echo "audio directory not found: $AUDIO_DIR" >&2; exit 1; }
[ -f "$HERE/remote_run_cpu.py" ] || {
  echo "missing $HERE/remote_run_cpu.py" >&2; exit 1
}

mapfile -t MP3S < <(find "$AUDIO_DIR" -maxdepth 1 -type f -iname '*.mp3' -print | sort)
[ "${#MP3S[@]}" -gt 0 ] || { echo "no MP3 files in $AUDIO_DIR" >&2; exit 1; }

echo "remote:      $REMOTE"
echo "audio dir:   $AUDIO_DIR"
echo "midi dir:    $MIDI_DIR"
echo "remote root: $REMOTE_ROOT"
echo "workers:     $JOBS x $THREADS threads"
echo "tracks:      ${#MP3S[@]}"

ssh -o BatchMode=yes "$REMOTE" "
  set -e
  test -x '$REMOTE_PY' || { echo 'remote python not found: $REMOTE_PY' >&2; exit 1; }
  test -d '$REMOTE_ROOT' || { echo 'remote Aria-AMT root not found: $REMOTE_ROOT' >&2; exit 1; }
  test -f '$REMOTE_ROOT/$CHECKPOINT' || { echo 'remote checkpoint not found: $CHECKPOINT' >&2; exit 1; }
  mkdir -p '$REMOTE_INPUT' '$REMOTE_OUTPUT'
"

# Copy the runner and the source audio.  tar avoids an rsync dependency and
# preserves Unicode filenames.
scp -q "$HERE/remote_run_cpu.py" "$REMOTE:$REMOTE_ROOT/run_cpu.py"
(
  cd "$AUDIO_DIR"
  tar -cf - -- *.mp3
) | ssh -o BatchMode=yes "$REMOTE" "
  set -e
  rm -rf '$REMOTE_INPUT'
  mkdir -p '$REMOTE_INPUT'
  tar -C '$REMOTE_INPUT' -xf -
"

echo "launching remote workers ..."
ssh -o BatchMode=yes "$REMOTE" \
  "cd '$REMOTE_ROOT' && REMOTE_PY='$REMOTE_PY' THREADS='$THREADS' JOBS='$JOBS' CHECKPOINT='$CHECKPOINT' INPUT='$REMOTE_INPUT' OUTPUT='$REMOTE_OUTPUT' FORCE='$FORCE' bash -s" <<'REMOTE_SCRIPT'
set -euo pipefail
mkdir -p "$OUTPUT"
if [ "$FORCE" = "1" ]; then
  rm -f "$OUTPUT"/*.mid "$OUTPUT"/*.log "$OUTPUT"/*.rc 2>/dev/null || true
fi
running=0
for f in "$INPUT"/*.mp3; do
  [ -f "$f" ] || continue
  b="$(basename "$f" .mp3)"
  if [ "$FORCE" != "1" ] && [ -f "$OUTPUT/$b.mid" ]; then
    echo "SKIP $(date +%H:%M:%S) $b (existing; use --force to overwrite)"
    continue
  fi
  while [ "$running" -ge "$JOBS" ]; do
    wait -n || true
    running=$((running - 1))
  done
  (
    echo "START $(date +%H:%M:%S) $b"
    "$REMOTE_PY" run_cpu.py \
      --threads "$THREADS" \
      --checkpoint "$CHECKPOINT" \
      --audio "$f" \
      --out "$OUTPUT/$b.mid" \
      > "$OUTPUT/$b.log" 2>&1
    rc=$?
    echo "$rc" > "$OUTPUT/$b.rc"
    echo "END $(date +%H:%M:%S) $b rc=$rc"
  ) &
  running=$((running + 1))
done
wait
printf '%s\n' "--- remote result ---"
for f in "$OUTPUT"/*.mid; do
  [ -f "$f" ] && ls -lh "$f"
done
for f in "$OUTPUT"/*.rc; do
  [ -f "$f" ] && echo "$(basename "$f" .rc): rc=$(cat "$f")"
done
REMOTE_SCRIPT

mkdir -p "$MIDI_DIR" "$MIDI_DIR/logs"
ssh -o BatchMode=yes "$REMOTE" \
  "cd '$REMOTE_OUTPUT' && tar -cf - -- *.mid" | tar -C "$MIDI_DIR" -xf -
ssh -o BatchMode=yes "$REMOTE" \
  "cd '$REMOTE_OUTPUT' && tar -cf - -- *.log" | tar -C "$MIDI_DIR/logs" -xf -

printf '%s\n' "--- local MIDI ---"
find "$MIDI_DIR" -maxdepth 1 -type f -name '*.mid' -printf '%s\t%p\n' | sort -k2
echo "done"
