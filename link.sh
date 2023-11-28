#!/bin/bash

set -o nounset # Treat unset variables as an error

ln -sf "$(pwd)"/zsh/zshrc.zsh "$HOME"/.zshrc
ln -sf "$(pwd)"/eslintrc.yaml "$HOME"/.eslintrc.yaml
ln -sf "$(pwd)"/prettierrc.yaml "$HOME"/.prettierrc.yaml
ln -sf "$(pwd)"/stylelintrc.yaml "$HOME"/.stylelintrc.yaml
ln -sf "$(pwd)"/clang-format.yaml "$HOME"/.clang-format
ln -sf "$(pwd)"/clang-tidy.yaml "$HOME"/.clang-tidy
ln -sf "$(pwd)"/golangci.yml "$HOME"/.golangci.yml
ln -sf "$(pwd)"/wezterm.lua "$HOME"/.wezterm.lua

configHome="${XDG_CONFIG_HOME:-$HOME/.config}"
ln -sf "$(pwd)"/config/emacs "$configHome"/emacs
ln -sf "$(pwd)"/config/nvim "$configHome"/nvim
ln -sf "$(pwd)"/config/lsd "$configHome"/lsd
ln -sf "$(pwd)"/config/starship.toml "$configHome"/starship.toml

# gitconfig
addGit="\n[include]\n\tpath = ${HOME}/myDoc/dotfiles/gitconfig.inc"
gitConf="${HOME}/.gitconfig"
if [ ! -f "${gitConf}" ]; then
	echo "${addGit}" >"${gitConf}"
else
	echo "${addGit}" >>"${gitConf}"
fi
