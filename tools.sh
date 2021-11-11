#!/bin/bash

# Packages
# https://github.com/ibraheemdev/modern-unix
packages=(
    # git
    # zsh
    bat
    fd
    fzf
    ripgrep
    exa
    stylua
    git-delta
    zoxide
    shfmt
    selene
    shellcheck
    starship

    # Fonts
)

function install() {
    archi=$(uname -s)
    for p in "${packages[@]}"; do
        case "$archi" in
        Darwin) brew install "$p" ;;
        Linux) sudo dnf install -y "${p}" ;;
        esac
    done
}

function main() {
    install
}

main
