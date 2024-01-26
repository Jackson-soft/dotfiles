#!/bin/bash

# Packages
packages=(
    # modern tools
    bat
    fd
    ripgrep
    eza
    git-extras
    bottom
    procs
    tealdeer
    pandoc
    ruff
    shfmt
    shellcheck
    jq
    hadolint
)

npms=(
    # formator
    prettier
    # lsp
    yaml-language-server
    bash-language-server
    vscode-langservers-extracted      # css json html eslint
    dockerfile-language-server-nodejs # docker
    pyright                           # python
    dot-language-server               # dot
)

function npmTool() {
    for p in "${npms[@]}"; do
        npm i -g "${p}"
    done
}

function check() {
    # Check brew
    if ! command -v brew >/dev/null 2>&1; then
        xcode-select --install
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi
}

function brewTool() {
    for app in "${packages[@]}"; do
        brew install -q "${app}"
    done
    brew cleanup
}

# should use sudo
function dnfTool() {
    for app in "${packages[@]}"; do
        dnf install -y "${app}"
    done
}

function main() {
    OS=$(uname -s)
    if [ "$OS" == "Linux" ]; then
        echo "This is a Linux system."
        dnfTool
    elif [ "$OS" == "Darwin" ]; then
        echo "This is a macOS system."
        check
        brewTool
    else
        echo "Unknown operating system."
    fi
}

main
