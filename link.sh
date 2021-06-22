#!/bin/bash

set -o nounset # Treat unset variables as an error

ln -s $(pwd)/zshrc $HOME/.zshrc
ln -s $(pwd)/vimrc $HOME/.vimrc
ln -s $(pwd)/eslintrc.yaml $HOME/.eslintrc.yaml
ln -s $(pwd)/prettierrc.yaml $HOME/.prettierrc.yaml
ln -s $(pwd)/stylelintrc.yaml $HOME/.stylelintrc.yaml
ln -s $(pwd)/clang-format.yaml $HOME/.clang-format
ln -s $(pwd)/clang-tidy.yaml $HOME/.clang-tidy

ln -s $(pwd)/gitconfig_global $HOME/.gitconfig_global
ln -s $(pwd)/emacs.d $HOME/.emacs.d
