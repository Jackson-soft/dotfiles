#!/bin/zsh

## Options section
setopt correct                                                  # Auto correct mistakes
setopt extendedglob                                             # Extended globbing. Allows using regular expressions with *
setopt nocaseglob                                               # Case insensitive globbing
setopt rcexpandparam                                            # Array expension with parameters
setopt nocheckjobs                                              # Don't warn about running processes when exiting
setopt numericglobsort                                          # Sort filenames numerically when it makes sense
setopt nobeep                                                   # No beep
setopt appendhistory                                            # Immediately append history instead of overwriting
setopt histignorealldups                                        # If a new command is a duplicate, remove the older one
setopt autocd                                                   # if only directory path is entered, cd there.
setopt inc_append_history                                       # save commands are added to the history immediately, otherwise only when shell exits.
setopt histignorespace                                          # Don't save commands that start with space
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
setopt always_to_end          # when completing from the middle of a word, move the cursor to the end of the word
setopt list_ambiguous         # complete as much of a completion until it gets ambiguous.
setopt auto_pushd
setopt pushd_ignore_dups
setopt pushdminus

# Set editor default keymap to emacs (`-e`) or vi (`-v`)
bindkey -e

alias -g ...='../..'

# fzf
# https://github.com/junegunn/fzf
export FZF_DEFAULT_COMMAND="fd -t f -H -L -E '.git' || rg --files --hidden --follow --glob '!.git' || find ."
export FZF_CTRL_T_COMMAND=$FZF_DEFAULT_COMMAND
export FZF_ALT_C_COMMAND="fd -t d"
export FZF_CTRL_R_OPTS='--sort --exact'
export FZF_DEFAULT_OPTS="
       --layout=reverse
       --info=inline
       --height=~70%
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

# Color man pages
export LESS_TERMCAP_mb=$'\E[01;32m'
export LESS_TERMCAP_md=$'\E[01;32m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;47;34m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;36m'
export LESS=-R
