#!/usr/bin/env bash
# Prepare piano stems for Aria-AMT with Demucs.
#
# Demucs 6-source model writes drums/bass/other/vocals/guitar/piano.
# The piano stem is copied to stems/piano/<name>.wav for easy Aria input.
set -euo pipefail

PYTHON="${IGEM_PYTHON:-python3}"
AUDIO_DIR="${IGEM_AUDIO_DIR:-/home/cjc/Downloads/iGEM 配乐}"
OUT_DIR="${IGEM_STEM_DIR:-${AUDIO_DIR}/stems}"
MODEL="${IGEM_DEMUCS_MODEL:-htdemucs_6s}"
DEVICE="${IGEM_DEMUCS_DEVICE:-cpu}"
EXT="${IGEM_AUDIO_EXT:-mp3}"
FORCE=0
TRACKS=""

usage() {
  cat <<EOF
Usage: $(basename "$0") [options]

Options:
  --audio-dir DIR     source audio directory (default: $AUDIO_DIR)
  --out-dir DIR       stem output directory (default: $OUT_DIR)
  --model NAME        Demucs model (default: $MODEL)
  --device DEVICE     cpu or cuda (default: $DEVICE)
  --extension EXT     source extension without dot (default: $EXT)
  --tracks LIST       comma-separated basenames; default: all
  --force             redo existing stems
  -h, --help          show this help
EOF
}

while (($#)); do
  case "$1" in
    --audio-dir) AUDIO_DIR="$2"; shift 2 ;;
    --out-dir) OUT_DIR="$2"; shift 2 ;;
    --model) MODEL="$2"; shift 2 ;;
    --device) DEVICE="$2"; shift 2 ;;
    --extension) EXT="$2"; shift 2 ;;
    --tracks) TRACKS="$2"; shift 2 ;;
    --force) FORCE=1; shift ;;
    -h|--help) usage; exit 0 ;;
    *) echo "unknown option: $1" >&2; usage >&2; exit 2 ;;
  esac
done

[ -d "$AUDIO_DIR" ] || { echo "audio directory not found: $AUDIO_DIR" >&2; exit 1; }
"$PYTHON" -c 'import demucs.separate' >/dev/null 2>&1 || {
  echo "Demucs is not installed for $PYTHON" >&2
  echo "install it with:" >&2
  echo "  $PYTHON -m pip install demucs" >&2
  exit 1
}
mkdir -p "$OUT_DIR" "$OUT_DIR/piano"

mapfile -t FILES < <(find "$AUDIO_DIR" -maxdepth 1 -type f -iname "*.${EXT}" -print | sort)
[ "${#FILES[@]}" -gt 0 ] || { echo "no .$EXT files in $AUDIO_DIR" >&2; exit 1; }

for src in "${FILES[@]}"; do
  name="$(basename "$src")"
  name="${name%.*}"
  if [ -n "$TRACKS" ]; then
    case ",$TRACKS," in
      *",$name,"*) ;;
      *) continue ;;
    esac
  fi
  piano="$OUT_DIR/piano/$name.wav"
  if [ "$FORCE" != "1" ] && [ -f "$piano" ]; then
    echo "skip existing: $piano"
    continue
  fi
  echo "=== $name ==="
  "$PYTHON" -m demucs.separate \
    -n "$MODEL" \
    --device "$DEVICE" \
    -o "$OUT_DIR" \
    "$src"
  demucs_piano="$OUT_DIR/$MODEL/$name/piano.wav"
  if [ ! -f "$demucs_piano" ]; then
    echo "Demucs did not produce expected piano stem: $demucs_piano" >&2
    exit 1
  fi
  cp -f "$demucs_piano" "$piano"
  echo "piano stem: $piano"
done

echo "done: $OUT_DIR/piano"
