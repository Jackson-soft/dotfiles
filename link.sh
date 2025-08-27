#!/bin/bash

set -o nounset # Treat unset variables as an error

ln -sf "$(pwd)"/zsh/zshrc.zsh "$HOME"/.zshrc
ln -sf "$(pwd)"/zsh/p10k.zsh "$HOME"/.p10k.zsh
ln -sf "$(pwd)"/prettierrc.yaml "$HOME"/.prettierrc.yaml
ln -sf "$(pwd)"/clang-format.yaml "$HOME"/.clang-format
ln -sf "$(pwd)"/clang-tidy.yaml "$HOME"/.clang-tidy
ln -sf "$(pwd)"/golangci.yml "$HOME"/.golangci.yml
# ln -sf "$(pwd)"/wezterm.lua "$HOME"/.wezterm.lua
# ln -sf "$(pwd)"/eslintrc.yaml "$HOME"/.eslintrc.yaml

configHome="${XDG_CONFIG_HOME:-$HOME/.config}"
ln -sf "$(pwd)"/config/emacs "$configHome"/emacs
ln -sf "$(pwd)"/config/nvim "$configHome"/nvim
ln -sf "$(pwd)"/config/sqlfluff.cfg "$configHome"/sqlfluff

# gitconfig
gitConf="${HOME}/.gitconfig"
includePath="${HOME}/myDoc/dotfiles/gitconfig.inc"

# 确保文件存在
touch "${gitConf}" || exit 1

# 检查配置是否已存在
if grep -qF "path = ${includePath}" "${gitConf}"; then
    echo "配置已存在，无需重复添加."
    exit 0
fi

# 使用 printf 安全写入配置（自动处理转义）
if printf '\n[include]\n\tpath = %s\n' "${includePath}" >>"${gitConf}"; then
    echo "Git 配置已成功更新!"
else
    echo "错误：写入配置失败!" >&2
    exit 1
fi
