#!/bin/zsh

## configuration
setopt extended_history       # record timestamp of command in HISTFILE
setopt hist_expire_dups_first # delete duplicates first when HISTFILE size exceeds HISTSIZE
setopt hist_ignore_dups       # ignore duplicated commands history list
setopt hist_ignore_space      # ignore commands that start with space
setopt hist_verify            # show command with history expansion to user before running it
setopt share_history          # share command history data
setopt auto_menu              # show completion menu on successive tab press
setopt hash_list_all          # hash everything before completion
setopt completealiases        # complete alisases
setopt complete_in_word       # allow completion from within a word/phrase
setopt nocorrect              # spelling correction for commands
setopt always_to_end          # when completing from the middle of a word, move the cursor to the end of the word
setopt list_ambiguous         # complete as much of a completion until it gets ambiguous.
setopt auto_pushd
setopt auto_cd
setopt pushd_ignore_dups
setopt pushdminus

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
