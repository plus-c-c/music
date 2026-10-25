#!/bin/bash
# =============================================================================
# download-libraries.sh — 下载免费 SF2/SFZ 音色库
# 用法: ./download-libraries.sh [all|sf2|sfz|clean]
# =============================================================================

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SF2_DIR="$SCRIPT_DIR/sf2"
SFZ_DIR="$SCRIPT_DIR/sfz"
SAMPLES_DIR="$SCRIPT_DIR/samples"

# 颜色输出
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

log()  { echo -e "${GREEN}[GET]${NC} $1"; }
warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
err()  { echo -e "${RED}[ERR]${NC} $1"; exit 1; }

download() {
    local url="$1"
    local dest="$2"
    if [ -f "$dest" ]; then
        warn "已存在: $(basename "$dest")"
        return 0
    fi
    log "下载: $(basename "$dest")"
    if command -v curl &>/dev/null; then
        curl -L --progress-bar -o "$dest" "$url"
    elif command -v wget &>/dev/null; then
        wget -q --show-progress -O "$dest" "$url"
    else
        err "需要 curl 或 wget"
    fi
}

extract() {
    local archive="$1"
    local dest="$2"
    log "解压: $(basename "$archive") -> $dest"
    mkdir -p "$dest"
    case "$archive" in
        *.tar.gz|*.tgz) tar xzf "$archive" -C "$dest" --strip-components=1 ;;
        *.tar.bz2)      tar xjf "$archive" -C "$dest" --strip-components=1 ;;
        *.zip)           unzip -qo "$archive" -d "$dest" ;;
        *)               cp "$archive" "$dest/" ;;
    esac
}

# =============================================================================
# SF2 音色库 (SoundFont 2)
# =============================================================================
download_sf2() {
    mkdir -p "$SF2_DIR"
    echo ""
    echo "=== 下载 SF2 音色库 ==="

    # Arachno General MIDI — 经典 GM 音色集
    download "http://www.arachnosoftware.net/~soundfonts/Arachno%20SoundFont%20-%20Version%201.0.sf2" \
             "$SF2_DIR/arachno-gm.sf2"

    # FluidR3 GM — 高质量 GM 音色
    download "https://www.hammersound.net/sounds/FluidR3_GM.sf2" \
             "$SF2_DIR/fluidr3-gm.sf2" || warn "FluidR3 需手动下载: https://www.hammersound.net/sounds/FluidR3_GM.sf2"

    # GeneralUser GS — 通用 MIDI 音色
    download "https://github.com/DeMCore/GeneralUser/raw/master/GeneralUser%20GS%20v1.471/GeneralUser%20GS%20v1.471.sf2" \
             "$SF2_DIR/generaluser-gs.sf2" || warn "GeneralUser 需手动下载"

    echo "SF2 音色库下载完成"
    ls -lh "$SF2_DIR"/*.sf2 2>/dev/null || echo "  (无文件)"
}

# =============================================================================
# SFZ 音色库
# =============================================================================
download_sfz() {
    mkdir -p "$SFZ_DIR"
    echo ""
    echo "=== 下载 SFZ 音色库 ==="

    # Sonatina Symphonic Orchestra — 交响乐采样
    local SSO_URL="https://github.com/peoro/SonatinaSymphonicOrchestra/archive/refs/heads/master.zip"
    download "$SSO_URL" "$SFZ_DIR/sso-temp.zip"
    if [ -f "$SFZ_DIR/sso-temp.zip" ]; then
        extract "$SFZ_DIR/sso-temp.zip" "$SFZ_DIR/sonatina-orchestra"
        rm -f "$SFZ_DIR/sso-temp.zip"
    fi

    # Virtual Playing Orchestra
    local VPO_URL="http://virtualplaying.com/virtual-playing-orchestra/VirtualPlayingOrchestral3.zip"
    download "$VPO_URL" "$SFZ_DIR/vpo-temp.zip" || warn "VPO 需手动下载: http://virtualplaying.com/"
    if [ -f "$SFZ_DIR/vpo-temp.zip" ]; then
        extract "$SFZ_DIR/vpo-temp.zip" "$SFZ_DIR/virtual-playing-orchestra"
        rm -f "$SFZ_DIR/vpo-temp.zip"
    fi

    # Band Instruments SFZ
    local BAND_URL="http://www.bandshed.net/sounds/sfz/band-instruments.zip"
    download "$BAND_URL" "$SFZ_DIR/band-temp.zip" || warn "Band instruments 需手动下载"
    if [ -f "$SFZ_DIR/band-temp.zip" ]; then
        extract "$SFZ_DIR/band-temp.zip" "$SFZ_DIR/band-instruments"
        rm -f "$SFZ_DIR/band-temp.zip"
    fi

    echo "SFZ 音色库下载完成"
    find "$SFZ_DIR" -name "*.sfz" | head -20
}

# =============================================================================
# 清理
# =============================================================================
clean() {
    echo "清理下载的临时文件..."
    find "$SF2_DIR" "$SFZ_DIR" "$SAMPLES_DIR" -name "*.tmp" -o -name "*.temp" -o -name "*.zip" 2>/dev/null | xargs rm -f
    echo "完成"
}

# =============================================================================
# 主入口
# =============================================================================
case "${1:-all}" in
    sf2)   download_sf2 ;;
    sfz)   download_sfz ;;
    all)   download_sf2; download_sfz ;;
    clean) clean ;;
    *)
        echo "用法: $0 [all|sf2|sfz|clean]"
        echo ""
        echo "  all   — 下载所有音色库 (默认)"
        echo "  sf2   — 仅下载 SF2 音色库"
        echo "  sfz   — 仅下载 SFZ 音色库"
        echo "  clean — 清理临时文件"
        exit 1
        ;;
esac
