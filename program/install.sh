#!/bin/bash
set -euo pipefail

# ============================================================================
# Package lists
# ============================================================================
tools=(
    # modern unix
    bat
    ripgrep
    eza
    tealdeer
    pandoc
    ruff
    shfmt
    shellcheck
    jq
    hadolint
    ugrep
    graphviz # dot
    neovim
)

npms=(
    # formatter
    prettier
    # lsp
    yaml-language-server
    bash-language-server
    vscode-langservers-extracted      # css json html eslint
    dockerfile-language-server-nodejs # docker
)

# ============================================================================
# Helpers
# ============================================================================
info() { printf '\033[1;34m[INFO]\033[0m  %s\n' "$*"; }
warn() { printf '\033[1;33m[WARN]\033[0m  %s\n' "$*" >&2; }
error() {
    printf '\033[1;31m[ERROR]\033[0m %s\n' "$*" >&2
    exit 1
}

# ============================================================================
# macOS
# ============================================================================
ensure_brew() {
    if command -v brew &>/dev/null; then return; fi
    info "Installing Homebrew …"
    xcode-select --install 2>/dev/null || true
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
}

install_brew() {
    ensure_brew
    info "Installing packages via brew …"
    brew install -q "${tools[@]}"
    brew cleanup
}

# ============================================================================
# Linux (Fedora / dnf5)
# ============================================================================
install_dnf() {
    info "Installing packages via dnf5 …"
    sudo dnf install -y "${tools[@]}"
}

# ============================================================================
# npm (cross-platform)
# ============================================================================
install_npm() {
    if ! command -v npm &>/dev/null; then
        warn "npm not found, skipping npm packages."
        return
    fi
    info "Installing npm global packages …"
    npm i -g "${npms[@]}"
}

# ============================================================================
# Main
# ============================================================================
main() {
    local os
    os=$(uname -s)
    case "$os" in
    Darwin)
        info "Detected macOS"
        install_brew
        ;;
    Linux)
        info "Detected Linux"
        install_dnf
        ;;
    *)
        error "Unsupported OS: $os"
        ;;
    esac
    install_npm
}

main "$@"
