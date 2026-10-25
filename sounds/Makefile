# =============================================================================
# sounds/Makefile — 音色库自动化安装
# =============================================================================

SHELL := /bin/bash

.PHONY: all sf2 sfz presets clean list help check

# 默认目标
all: check sf2 sfz
	@echo ""
	@echo "=== 音色库安装完成 ==="
	@echo "SF2 文件:  $$(find sf2 -name '*.sf2' 2>/dev/null | wc -l) 个"
	@echo "SFZ 目录:  $$(find sfz -name '*.sfz' 2>/dev/null | wc -l) 个"
	@echo "Ardour预设: $$(find ardour-presets -name '*.fxp' 2>/dev/null | wc -l) 个"
	@echo ""
	@echo "使用方法:"
	@echo "  racket -l music/sounds/sf2-loader"

# 检查依赖
check:
	@echo "检查依赖..."
	@command -v curl >/dev/null 2>&1 || command -v wget >/dev/null 2>&1 || \
		(echo "需要 curl 或 wget"; exit 1)
	@command -v unzip >/dev/null 2>&1 || \
		(echo "需要 unzip"; exit 1)

# 下载 SF2 音色库
sf2:
	@echo "=== 下载 SF2 音色库 ==="
	@bash ./download-libraries.sh sf2

# 下载 SFZ 音色库
sfz:
	@echo "=== 下载 SFZ 音色库 ==="
	@bash ./download-libraries.sh sfz

# 提取 Ardour 预设
presets:
	@echo "=== 提取 Ardour 预设 ==="
	@bash ./extract-ardour-presots.sh extract || true

# 列出所有可用音色
list:
	@echo "=== 可用 SF2 音色库 ==="
	@find sf2 -name "*.sf2" -exec ls -lh {} \; 2>/dev/null || echo "  (无)"
	@echo ""
	@echo "=== 可用 SFZ 音色库 ==="
	@find sfz -name "*.sfz" | head -20 2>/dev/null || echo "  (无)"
	@echo ""
	@echo "=== Ardour 预设 ==="
	@find ardour-presets -name "*.fxp" | wc -l | xargs -I{} echo "  {} 个预设"

# 清理
clean:
	@echo "清理..."
	@bash ./download-libraries.sh clean
	@rm -rf ardour-presets/*.fxp

# 帮助
help:
	@echo "音色库管理工具"
	@echo ""
	@echo "目标:"
	@echo "  make all     — 下载所有音色库 (默认)"
	@echo "  make sf2     — 仅下载 SF2"
	@echo "  make sfz     — 仅下载 SFZ"
	@echo "  make presets — 提取 Ardour 预设"
	@echo "  make list    — 列出可用音色"
	@echo "  make clean   — 清理临时文件"
	@echo "  make check   — 检查依赖"
