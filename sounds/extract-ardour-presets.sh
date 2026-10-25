#!/bin/bash
# =============================================================================
# extract-ardour-presets.sh — 提取 Ardour VST2 插件预设
# 用法: ./extract-ardour-presets.sh [ardour-version]
# =============================================================================

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
OUTPUT_DIR="$SCRIPT_DIR/ardour-presets"
ARDOUR_VERSION="${1:-6}"

# 预设目录 (Linux)
PRESET_DIR="$HOME/.config/ardour${ARDOUR_VERSION}/presets"

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

log()  { echo -e "${GREEN}[EXTRACT]${NC} $1"; }
warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
err()  { echo -e "${RED}[ERR]${NC} $1"; exit 1; }

# 检查 ardour2fxp 是否可用
check_ardour2fxp() {
    if command -v ardour2fxp &>/dev/null; then
        return 0
    fi

    warn "ardour2fxp 未安装，尝试安装..."
    if command -v pip &>/dev/null; then
        pip install ardour2fxp
    elif command -v pip3 &>/dev/null; then
        pip3 install ardour2fxp
    else
        err "请先安装 ardour2fxp: pip install ardour2fxp"
    fi
}

# 提取所有预设
extract_presets() {
    if [ ! -d "$PRESET_DIR" ]; then
        err "Ardour 预设目录不存在: $PRESET_DIR"
    fi

    local count=$(find "$PRESET_DIR" -name "vst-*" -type f | wc -l)
    if [ "$count" -eq 0 ]; then
        warn "未找到 VST2 预设文件"
        return 0
    fi

    log "找到 $count 个 VST2 预设文件"
    mkdir -p "$OUTPUT_DIR"

    for preset_file in "$PRESET_DIR"/vst-*; do
        local plugin_id=$(basename "$preset_file" | sed 's/vst-//')
        log "转换插件: $plugin_id"
        ardour2fxp -o "$OUTPUT_DIR" "$preset_file" 2>/dev/null || true
    done

    local total=$(find "$OUTPUT_DIR" -name "*.fxp" -type f | wc -l)
    log "提取完成: $total 个预设"
    find "$OUTPUT_DIR" -name "*.fxp" -type f | head -20
}

# 列出可用的预设
list_presets() {
    if [ ! -d "$PRESET_DIR" ]; then
        err "Ardour 预设目录不存在: $PRESET_DIR"
    fi

    echo "=== Ardour VST2 预设 (版本 $ARDOUR_VERSION) ==="
    echo ""

    for preset_file in "$PRESET_DIR"/vst-*; do
        [ -f "$preset_file" ] || continue
        local plugin_id=$(basename "$preset_file" | sed 's/vst-//')
        local count=$(grep -c "<preset" "$preset_file" 2>/dev/null || echo "0")
        echo "  插件 $plugin_id: $count 个预设"
    done
}

# 主入口
case "${1:-extract}" in
    extract)
        check_ardour2fxp
        extract_presets
        ;;
    list)
        list_presets
        ;;
    *)
        echo "用法: $0 [extract|list|<ardour-version>]"
        echo ""
        echo "  extract — 提取预设到 ardour-presets/ (默认)"
        echo "  list    — 列出可用预设"
        echo "  6       — 指定 Ardour 版本 (5/6/7)"
        exit 1
        ;;
esac
