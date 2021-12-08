#!/bin/zsh

if whence dircolors >/dev/null && ls --version &>/dev/null; then
    # GNU
    # ls colours
    if [[ -s ${HOME}/.dir_colors ]]; then
        eval "$(dircolors --sh ${HOME}/.dir_colors)"
    elif (( ! ${+LS_COLORS} )); then
        export LS_COLORS='di=1;34:ln=35:so=32:pi=33:ex=31:bd=1;36:cd=1;33:su=30;41:sg=30;46:tw=30;42:ow=30;43'
    fi
else
    # BSD
    # ls colours
    if (( ! ${+CLICOLOR} )) export CLICOLOR=1
    if (( ! ${+LSCOLORS} )) export LSCOLORS='ExfxcxdxbxGxDxabagacad'
fi

# see https://thevaluable.dev/zsh-completion-guide-examples
# cd 不区分大小写
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' '+r:|?=**'
zstyle ':completion:*' menu select
zstyle ':completion:*' file-sort modification
zstyle ':completion:*:git-checkout:*' sort false
zstyle ':completion:*' verbose yes
zstyle ':completion:*' squeeze-slashes true
# Directories
if (( ${+LS_COLORS} )); then
    zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
else
    zstyle ':completion:*:default' list-colors ${(s.:.):-di=1;34:ln=35:so=32:pi=33:ex=31:bd=1;36:cd=1;33:su=30;41:sg=30;46:tw=30;42:ow=30;43}
fi
# Enable caching
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "${XDG_CACHE_HOME:-$HOME/.cache}/zsh/zcompcache"
zstyle ':completion:*:descriptions' format '[%d]'

alias -g ...='../..'

# Set editor default keymap to emacs (`-e`) or vi (`-v`)
bindkey -e

# colored-man-pages
man() {
    env \
        LESS_TERMCAP_md=$(tput bold; tput setaf 4) \
        LESS_TERMCAP_me=$(tput sgr0) \
        LESS_TERMCAP_mb=$(tput blink) \
        LESS_TERMCAP_us=$(tput setaf 2) \
        LESS_TERMCAP_ue=$(tput sgr0) \
        LESS_TERMCAP_so=$(tput smso) \
        LESS_TERMCAP_se=$(tput rmso) \
        PAGER="${commands[less]:-$PAGER}" \
        man "$@"
}
