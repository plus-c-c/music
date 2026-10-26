#!/usr/bin/env bash
# Run Aria-AMT on the AMD ROCm GPU on the iGEM server.
set -euo pipefail

REMOTE="${IGEM_REMOTE:-igem}"
REMOTE_ROOT="${IGEM_GPU_ROOT:-/home/igem/projects/aria-amt-gpu}"
REMOTE_PY="${IGEM_GPU_PY:-/home/igem/.local/miniconda3/envs/demucs-rocm/bin/python}"
CHECKPOINT="${IGEM_GPU_CHECKPOINT:-/home/igem/projects/aria-amt/checkpoints/piano-medium-double-1.0.safetensors}"
AUDIO_DIR="${IGEM_AUDIO_DIR:-/home/cjc/Downloads/iGEM 配乐}"
MIDI_DIR="${IGEM_GPU_MIDI_DIR:-${AUDIO_DIR}/aria-amt-midi-gpu}"
AUDIO_EXT="${IGEM_AUDIO_EXT:-mp3}"
REMOTE_INPUT="${IGEM_GPU_INPUT:-${REMOTE_ROOT}/work/input}"
REMOTE_OUTPUT="${IGEM_GPU_OUTPUT:-${REMOTE_ROOT}/work/output}"
FORCE=0

usage() {
  cat <<EOF
Usage: $(basename "$0") [options]

Options:
  --audio-dir DIR       local source directory (default: $AUDIO_DIR)
  --audio-ext EXT       source extension without dot (default: $AUDIO_EXT)
  --midi-dir DIR        local MIDI output directory (default: $MIDI_DIR)
  --remote HOST         SSH host (default: $REMOTE)
  --remote-root DIR     remote GPU Aria checkout (default: $REMOTE_ROOT)
  --remote-python PATH  remote Python (default: $REMOTE_PY)
  --checkpoint PATH     absolute checkpoint path (default: $CHECKPOINT)
  --force               clear remote output before running
  -h, --help            show help
EOF
}

while (($#)); do
  case "$1" in
    --audio-dir) AUDIO_DIR="$2"; shift 2 ;;
    --audio-ext) AUDIO_EXT="$2"; shift 2 ;;
    --midi-dir) MIDI_DIR="$2"; shift 2 ;;
    --remote) REMOTE="$2"; shift 2 ;;
    --remote-root) REMOTE_ROOT="$2"; shift 2 ;;
    --remote-python) REMOTE_PY="$2"; shift 2 ;;
    --checkpoint) CHECKPOINT="$2"; shift 2 ;;
    --force) FORCE=1; shift ;;
    -h|--help) usage; exit 0 ;;
    *) echo "unknown option: $1" >&2; usage >&2; exit 2 ;;
  esac
done

command -v ssh >/dev/null || { echo "ssh is required" >&2; exit 1; }
command -v tar >/dev/null || { echo "tar is required" >&2; exit 1; }
[ -d "$AUDIO_DIR" ] || { echo "audio directory not found: $AUDIO_DIR" >&2; exit 1; }
mapfile -t FILES < <(find "$AUDIO_DIR" -maxdepth 1 -type f -iname "*.${AUDIO_EXT}" -print | sort)
[ "${#FILES[@]}" -gt 0 ] || { echo "no .$AUDIO_EXT files in $AUDIO_DIR" >&2; exit 1; }

ssh -o BatchMode=yes "$REMOTE" "
  set -e
  test -x '$REMOTE_PY' || { echo 'remote Python not found: $REMOTE_PY' >&2; exit 1; }
  test -f '$CHECKPOINT' || { echo 'checkpoint not found: $CHECKPOINT' >&2; exit 1; }
  test -f '$REMOTE_ROOT/amt/data.py' || { echo 'GPU Aria source not found: $REMOTE_ROOT' >&2; exit 1; }
  '$REMOTE_PY' -c 'import torch; assert torch.cuda.is_available()' || {
    echo 'ROCm torch.cuda is not available' >&2; exit 1; }
  mkdir -p '$REMOTE_INPUT' '$REMOTE_OUTPUT'
"

(
  cd "$AUDIO_DIR"
  tar -cf - -- *.$AUDIO_EXT
) | ssh -o BatchMode=yes "$REMOTE" "
  set -e
  rm -rf '$REMOTE_INPUT'
  mkdir -p '$REMOTE_INPUT'
  tar -C '$REMOTE_INPUT' -xf -
"

ssh -o BatchMode=yes "$REMOTE" \
  "cd '$REMOTE_ROOT' && REMOTE_PY='$REMOTE_PY' CHECKPOINT='$CHECKPOINT' INPUT='$REMOTE_INPUT' OUTPUT='$REMOTE_OUTPUT' FORCE='$FORCE' bash -s" <<'REMOTE_SCRIPT'
set -euo pipefail
if [ "$FORCE" = "1" ]; then
  rm -f "$OUTPUT"/*.mid 2>/dev/null || true
fi
echo "GPU:"
"$REMOTE_PY" - <<'PY'
import torch
print(torch.cuda.get_device_name(0))
print("torch", torch.__version__)
PY
echo "transcribing on GPU ..."
"$REMOTE_PY" -m amt.run transcribe medium-double "$CHECKPOINT" \
  -load_dir "$INPUT" \
  -save_dir "$OUTPUT" \
  -bs 1 \
  -num_workers 1
printf '%s\n' "--- remote MIDI ---"
for f in "$OUTPUT"/*.mid; do
  [ -f "$f" ] && ls -lh "$f"
done
true
REMOTE_SCRIPT

mkdir -p "$MIDI_DIR"
ssh -o BatchMode=yes "$REMOTE" \
  "cd '$REMOTE_OUTPUT' && tar -cf - -- *.mid" | tar -C "$MIDI_DIR" -xf -

printf '%s\n' "--- local GPU MIDI ---"
find "$MIDI_DIR" -maxdepth 1 -type f -name '*.mid' -printf '%s\t%p\n' | sort -k2
