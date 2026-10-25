#!/usr/bin/env bash
# Rebuild the current presentation background and voice-loop level comparison.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

python3 "$HERE/presentation_background.py"
python3 "$HERE/presentation_voice_loop.py" "$@"
