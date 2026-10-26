#!/usr/bin/env bash
# Run Demucs piano separation on the iGEM server and download piano stems.
set -euo pipefail

REMOTE="${IGEM_REMOTE:-igem}"
REMOTE_ROOT="${IGEM_DEMUCS_ROOT:-/home/igem/projects/demucs-work}"
REMOTE_PY="${IGEM_DEMUCS_PY:-/home/igem/.local/miniconda3/envs/demucs/bin/python}"
AUDIO_DIR="${IGEM_AUDIO_DIR:-/home/cjc/Downloads/iGEM 配乐}"
OUT_DIR="${IGEM_STEM_DIR:-${AUDIO_DIR}/stems/piano}"
MODEL="${IGEM_DEMUCS_MODEL:-htdemucs_6s}"
EXT="${IGEM_AUDIO_EXT:-mp3}"
THREADS="${IGEM_DEMUCS_THREADS:-8}"
FORCE=0
TRACKS=""

usage() {
  cat <<EOF
Usage: $(basename "$0") [options]

Options:
  --audio-dir DIR       local source directory (default: $AUDIO_DIR)
  --out-dir DIR         local piano stem directory (default: $OUT_DIR)
  --remote HOST         SSH host (default: $REMOTE)
  --remote-root DIR     remote Demucs work directory (default: $REMOTE_ROOT)
  --remote-python PATH  remote Demucs Python (default: $REMOTE_PY)
  --model NAME          Demucs model (default: $MODEL)
  --extension EXT       source extension without dot (default: $EXT)
  --threads N           CPU threads (default: $THREADS)
  --tracks LIST         comma-separated basenames; default: all
  --force               overwrite existing stems
  -h, --help            show help
EOF
}

while (($#)); do
  case "$1" in
    --audio-dir) AUDIO_DIR="$2"; shift 2 ;;
    --out-dir) OUT_DIR="$2"; shift 2 ;;
    --remote) REMOTE="$2"; shift 2 ;;
    --remote-root) REMOTE_ROOT="$2"; shift 2 ;;
    --remote-python) REMOTE_PY="$2"; shift 2 ;;
    --model) MODEL="$2"; shift 2 ;;
    --extension) EXT="$2"; shift 2 ;;
    --threads) THREADS="$2"; shift 2 ;;
    --tracks) TRACKS="$2"; shift 2 ;;
    --force) FORCE=1; shift ;;
    -h|--help) usage; exit 0 ;;
    *) echo "unknown option: $1" >&2; usage >&2; exit 2 ;;
  esac
done

command -v ssh >/dev/null || { echo "ssh is required" >&2; exit 1; }
command -v tar >/dev/null || { echo "tar is required" >&2; exit 1; }
[ -d "$AUDIO_DIR" ] || { echo "audio directory not found: $AUDIO_DIR" >&2; exit 1; }

mapfile -t FILES < <(find "$AUDIO_DIR" -maxdepth 1 -type f -iname "*.${EXT}" -print | sort)
[ "${#FILES[@]}" -gt 0 ] || { echo "no .$EXT files in $AUDIO_DIR" >&2; exit 1; }

# Filter selected tracks before upload.
UPLOAD=()
for f in "${FILES[@]}"; do
  b="$(basename "$f")"; b="${b%.*}"
  if [ -n "$TRACKS" ]; then
    case ",$TRACKS," in *",$b,"*) ;; *) continue ;; esac
  fi
  UPLOAD+=("$f")
done
[ "${#UPLOAD[@]}" -gt 0 ] || { echo "no selected audio files" >&2; exit 1; }

ssh -o BatchMode=yes "$REMOTE" "
  set -e
  test -x '$REMOTE_PY' || { echo 'remote Demucs python not found: $REMOTE_PY' >&2; exit 1; }
  '$REMOTE_PY' -c 'import demucs' || { echo 'Demucs missing in remote env' >&2; exit 1; }
  mkdir -p '$REMOTE_ROOT/input' '$REMOTE_ROOT/output' '$REMOTE_ROOT/piano'
"
(
  cd "$AUDIO_DIR"
  tar -cf - -- "${UPLOAD[@]#$AUDIO_DIR/}"
) | ssh -o BatchMode=yes "$REMOTE" "
  set -e
  rm -rf '$REMOTE_ROOT/input'
  mkdir -p '$REMOTE_ROOT/input'
  tar -C '$REMOTE_ROOT/input' -xf -
"

ssh -o BatchMode=yes "$REMOTE" \
  "cd '$REMOTE_ROOT' && REMOTE_PY='$REMOTE_PY' MODEL='$MODEL' EXT='$EXT' THREADS='$THREADS' FORCE='$FORCE' bash -s" <<'REMOTE_SCRIPT'
set -euo pipefail
for f in input/*."$EXT"; do
  [ -f "$f" ] || continue
  b="$(basename "$f")"; b="${b%.*}"
  echo "=== $b ==="
  if [ "$FORCE" != "1" ] && [ -f "piano/$b.wav" ]; then
    echo "skip existing"
    continue
  fi
  OMP_NUM_THREADS="$THREADS" MKL_NUM_THREADS="$THREADS" \
    "$REMOTE_PY" -m demucs.separate \
      -n "$MODEL" --device cpu -o output "$f"
  src="output/$MODEL/$b/piano.wav"
  [ -f "$src" ] || { echo "missing Demucs piano stem: $src" >&2; exit 1; }
  cp -f "$src" "piano/$b.wav"
done
true
REMOTE_SCRIPT

mkdir -p "$OUT_DIR"
ssh -o BatchMode=yes "$REMOTE" \
  "cd '$REMOTE_ROOT/piano' && tar -cf - -- *.wav" | tar -C "$OUT_DIR" -xf -

printf '%s\n' "--- piano stems ---"
find "$OUT_DIR" -maxdepth 1 -type f -name '*.wav' -printf '%s\t%p\n' | sort -k2
