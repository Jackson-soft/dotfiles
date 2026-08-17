#!/bin/bash
# Symlink dotfiles to their expected locations.
# Usage:    cd /path/to/dotfiles && ./link.sh
# Requires bash (uses associative arrays) - do not run via `sh link.sh`, which
# forces the sh interpreter regardless of this shebang. Use ./link.sh instead.
if [ -z "${BASH_VERSION:-}" ]; then
    echo "Error: this script requires bash. Run it as ./link.sh (not sh link.sh)." >&2
    exit 1
fi

set -euo pipefail

DOTFILES="$(cd "$(dirname "$0")" && pwd)"
CONFIG="${XDG_CONFIG_HOME:-$HOME/.config}"

# ── Symlinks ────────────────────────────────────────────────────────────────
# Parallel arrays (not an associative array) - macOS ships bash 3.2, which
# predates `declare -A` (bash 4.0+).
targets=(
    "$HOME/.zshrc"
    "$HOME/.prettierrc.yaml"
    "$HOME/.clang-format"
    "$HOME/.clang-tidy"
    "$HOME/.golangci.yml"
    "$CONFIG/emacs"
    "$CONFIG/nvim"
    "$CONFIG/sqlfluff"
    "$CONFIG/starship.toml"
)
sources=(
    "$DOTFILES/zsh/zshrc.zsh"
    "$DOTFILES/prettierrc.yaml"
    "$DOTFILES/clang-format.yaml"
    "$DOTFILES/clang-tidy.yaml"
    "$DOTFILES/golangci.yml"
    "$DOTFILES/config/emacs"
    "$DOTFILES/config/nvim"
    "$DOTFILES/config/sqlfluff"
    "$DOTFILES/config/starship.toml"
)

for i in "${!targets[@]}"; do
    target="${targets[$i]}"
    src="${sources[$i]}"
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
