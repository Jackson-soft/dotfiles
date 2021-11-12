#!/bin/bash

set -o nounset # Treat unset variables as an error

ln -sf "$(pwd)"/zsh/zshrc.zsh "$HOME"/.zshrc
ln -sf "$(pwd)"/eslintrc.yaml "$HOME"/.eslintrc.yaml
ln -sf "$(pwd)"/prettierrc.yaml "$HOME"/.prettierrc.yaml
ln -sf "$(pwd)"/stylelintrc.yaml "$HOME"/.stylelintrc.yaml
ln -sf "$(pwd)"/clang-format.yaml "$HOME"/.clang-format
ln -sf "$(pwd)"/clang-tidy.yaml "$HOME"/.clang-tidy
ln -sf "$(pwd)"/golangci.yml "$HOME"/.golangci.yml

ln -snf "$(pwd)"/emacs.d "$HOME"/.emacs.d

if [ ! -d "$HOME"/.config/nvim ]; then
    mkdir -p "$HOME"/.config/nvim
fi
ln -sf "$(pwd)"/init.lua "$HOME"/.config/nvim/init.lua

if [ ! -d "$HOME"/.config/kitty ]; then
    mkdir -p "$HOME"/.config/kitty
fi
ln -sf "$(pwd)"/kitty.conf "$HOME"/.config/kitty/kitty.conf

# gitconfig
addGit="\n[include]\n\tpath = ${HOME}/myDoc/dotfiles/gitconfig.inc"
gitConf="${HOME}/.gitconfig"
if [ ! -f "${gitConf}" ]; then
    echo "${addGit}" >"${gitConf}"
else
    echo "${addGit}" >>"${gitConf}"
fi
