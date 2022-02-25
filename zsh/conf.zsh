#!/bin/zsh

setopt share_history

# see https://thevaluable.dev/zsh-completion-guide-examples
# cd 不区分大小写
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' '+r:|?=**'
zstyle ':completion:*' menu select
zstyle ':completion:*' file-sort modification
zstyle ':completion:*:git-checkout:*' sort false
zstyle ':completion:*' verbose yes
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
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
