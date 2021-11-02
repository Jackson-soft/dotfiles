#!/bin/sh

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

install() {
	for p in ${packages[@]}; do
		brew install "$p"
	done
}

main() {
	install
}

main
