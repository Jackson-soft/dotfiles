#!/bin/zsh

## Options section
setopt EXTENDED_GLOB
setopt HIST_FCNTL_LOCK      # Use modern file-locking mechanisms, for better safety & performance.
setopt HIST_IGNORE_ALL_DUPS # Keep only the most recent copy of each duplicate entry in history.
setopt SHARE_HISTORY        # Auto-sync history between concurrent sessions.
setopt COMPLETE_IN_WORD     # Complete from both ends of a word.
setopt ALWAYS_TO_END        # Move cursor to the end of a completed word.
setopt PATH_DIRS            # Perform path search even on command names with slashes.
setopt AUTO_MENU            # Show completion menu on a successive tab press.
setopt AUTO_LIST            # Automatically list choices on ambiguous completion.
setopt AUTO_PARAM_SLASH     # If completed parameter is a directory, add a trailing slash.

# Set editor default keymap to emacs (`-e`) or vi (`-v`)
bindkey -e

alias -g ...='../..'

# fzf
# https://github.com/junegunn/fzf
export FZF_DEFAULT_COMMAND="fd -t f -H -L -E '.git' || rg --files --hidden --follow --glob '!.git' || find ."
export FZF_CTRL_T_COMMAND=$FZF_DEFAULT_COMMAND
export FZF_CTRL_T_OPTS="--preview '(bat --style=numbers --color=always {} || cat {} || eza --tree {}) 2> /dev/null | head -200'"
export FZF_ALT_C_OPTS="--preview '(eza --tree --icons --level 3 --color=always --group-directories-first {} || tree -NC {} || ls --color=always --group-directories-first {}) 2>/dev/null | head -200'"
export FZF_DEFAULT_OPTS="
       --height 80%
       --layout reverse
       --info inline
       --border block
       --exact
       --cycle
       --prompt '∼ '
       --marker '✓'
       --color 'dark,hl:33,hl+:37,fg+:235,bg+:136,fg+:254'
       --color 'info:254,prompt:37,spinner:108,pointer:235,marker:235'
       --preview-window :hidden
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

# see https://thevaluable.dev/zsh-completion-guide-examples
# zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' '+r:|?=**'
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
# Group matches and describe.
zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'
zstyle ':completion:*' rehash true
# fzf-tab
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:descriptions' format '[%d]'
zstyle ':fzf-tab:complete:_zlua:*' query-string input
zstyle ':fzf-tab:complete:kill:argument-rest' fzf-preview 'ps --pid=$word -o cmd --no-headers -w -w'
zstyle ':fzf-tab:complete:kill:argument-rest' fzf-flags '--preview-window=down:3:wrap'
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'lsd -1 --color=always $realpath'
zstyle ':fzf-tab:complete:cd:*' popup-pad 30 0
zstyle ":fzf-tab:*" fzf-flags --color=bg+:23
zstyle ':fzf-tab:*' switch-group ',' '.'
zstyle ':completion:*:git-checkout:*' sort false
zstyle ':completion:*' file-sort modification
zstyle ':completion::complete:*' use-cache on
zstyle ':completion:*:match:*' original only
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' verbose yes

zstyle ':completion:*' completer _expand_alias _expand _complete _ignored _approximate
zstyle ':completion:*' menu select=2
zstyle ':completion:*' special-dirs true
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
zstyle ':completion:*' accept-exact '*(N)'

# git
zstyle ':fzf-tab:complete:git-(add|diff|restore):*' fzf-preview \
	'git diff $word | delta'
zstyle ':fzf-tab:complete:git-log:*' fzf-preview \
	'git log --color=always $word'
zstyle ':fzf-tab:complete:git-help:*' fzf-preview \
	'git help $word | bat -plman --color=always'
zstyle ':fzf-tab:complete:git-show:*' fzf-preview \
	'case "$group" in
	"commit tag") git show --color=always $word ;;
	*) git show --color=always $word | delta ;;
	esac'
zstyle ':fzf-tab:complete:git-checkout:*' fzf-preview \
	'case "$group" in
	"modified file") git diff $word | delta ;;
	"recent commit object name") git show --color=always $word | delta ;;
	*) git log --color=always $word ;;
	esac'

# man
zstyle ':fzf-tab:complete:(\\|*/|)man:*' fzf-preview 'man $word'

# Kill
zstyle ':completion:*:*:*:*:processes' command 'ps -u $USER -o pid,user,comm -w -w'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;36=0=01'
zstyle ':completion:*:*:kill:*' force-list always
zstyle ':completion:*:*:kill:*' insert-ids single
