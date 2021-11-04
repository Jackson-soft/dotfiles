#!/bin/bash

set -o nounset # Treat unset variables as an error

ln -s "$(pwd)"/zsh/zshrc.zsh "$HOME"/.zshrc
ln -s "$(pwd)"/eslintrc.yaml "$HOME"/.eslintrc.yaml
ln -s "$(pwd)"/prettierrc.yaml "$HOME"/.prettierrc.yaml
ln -s "$(pwd)"/stylelintrc.yaml "$HOME"/.stylelintrc.yaml
ln -s "$(pwd)"/clang-format.yaml "$HOME"/.clang-format
ln -s "$(pwd)"/clang-tidy.yaml "$HOME"/.clang-tidy

ln -s "$(pwd)"/emacs.d "$HOME"/.emacs.d

if [ ! -d "$HOME"/.config/nvim ]; then
	mkdir -p "$HOME"/.config/nvim
fi
ln -s "$(pwd)"/init.lua "$HOME"/.config/nvim/init.lua

if [ ! -d "$HOME"/.config/kitty ]; then
	mkdir -p "$HOME"/.config/kitty
fi
ln -s "$(pwd)"/kitty.conf "$HOME"/.config/kitty/kitty.conf

ln -s "$(pwd)"/starship.toml "$HOME"/.config/starship.toml
