#!/bin/bash

# Packages
packages=(
	# git
	# zsh
	bat
	fd
	fzf
	ripgrep
	exa
	stylua
	delta
	zoxide
	shfmt
	selene
	shellcheck
	starship

	# Fonts
)

function install() {
	for p in "${packages[@]}"; do
		brew install "$p"
	done
}

function main() {
	install
}

main
