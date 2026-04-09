#!/bin/bash
# Symlink dotfiles to their expected locations.
# Usage: cd /path/to/dotfiles && ./link.sh
set -euo pipefail

DOTFILES="$(cd "$(dirname "$0")" && pwd)"
CONFIG="${XDG_CONFIG_HOME:-$HOME/.config}"

# ── Symlinks ────────────────────────────────────────────────────────────────
declare -A links=(
    ["$HOME/.zshrc"]="$DOTFILES/zsh/zshrc.zsh"
    ["$HOME/.p10k.zsh"]="$DOTFILES/zsh/p10k.zsh"
    ["$HOME/.prettierrc.yaml"]="$DOTFILES/prettierrc.yaml"
    ["$HOME/.clang-format"]="$DOTFILES/clang-format.yaml"
    ["$HOME/.clang-tidy"]="$DOTFILES/clang-tidy.yaml"
    ["$HOME/.golangci.yml"]="$DOTFILES/golangci.yml"
    ["$CONFIG/emacs"]="$DOTFILES/config/emacs"
    ["$CONFIG/nvim"]="$DOTFILES/config/nvim"
    ["$CONFIG/sqlfluff"]="$DOTFILES/config/sqlfluff.cfg"
)

for target in "${!links[@]}"; do
    src="${links[$target]}"
    mkdir -p "$(dirname "$target")"
    ln -sfn "$src" "$target"
    echo "  ${target} -> ${src}"
done

# ── Git include ─────────────────────────────────────────────────────────────
gitconf="$HOME/.gitconfig"
include_path="$DOTFILES/gitconfig.inc"

touch "$gitconf"
if ! grep -qF "path = ${include_path}" "$gitconf"; then
    printf '\n[include]\n\tpath = %s\n' "$include_path" >>"$gitconf"
    echo "  gitconfig include added."
else
    echo "  gitconfig include already present."
fi
