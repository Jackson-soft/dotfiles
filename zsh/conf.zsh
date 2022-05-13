#!/bin/zsh

setopt share_history

alias -g ...='../..'

# Set editor default keymap to emacs (`-e`) or vi (`-v`)
bindkey -e

# fzf
# https://github.com/junegunn/fzf
export FZF_DEFAULT_COMMAND="fd -t f -H -L -E '.git' || rg --files --hidden --follow --glob '!.git' || find ."
export FZF_CTRL_T_COMMAND=$FZF_DEFAULT_COMMAND
export FZF_ALT_C_COMMAND="fd -t d"
export FZF_CTRL_R_OPTS='--sort --exact'
export FZF_DEFAULT_OPTS="
       --layout=reverse
       --info=inline
       --height=70%
       --multi
       --border
       --cycle
       --preview-window=:hidden
       --preview '([[ -f {} ]] && (bat --style=numbers --color=always {} || cat {})) || ([[ -d {} ]] && (exa -T {} | less)) || echo {} 2> /dev/null | head -200'
       --prompt='∼ ' --marker='✓'
       --color='dark,hl:33,hl+:37,fg+:235,bg+:136,fg+:254'
       --color='info:254,prompt:37,spinner:108,pointer:235,marker:235'
       --bind '?:toggle-preview'
       "

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
