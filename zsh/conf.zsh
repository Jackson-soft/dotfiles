#!/bin/zsh

setopt share_history

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
