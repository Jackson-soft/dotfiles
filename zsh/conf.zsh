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
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' squeeze-slashes true
# Enable caching
zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path "${ZDOTDIR:-${HOME}}/.zcompcache"
zstyle ':completion:*:descriptions' format '[%d]'

archi=$(uname -s)
local fzfBase
case $archi in
    Darwin)
        export PATH=/usr/local/opt/llvm/bin:${PATH}:${HOME}/go/bin
        fzfBase=/usr/local/opt/fzf/shell ;;
    Linux)
        export PATH=${PATH}:${HOME}/go/bin
        fzfBase=/usr/share/fzf/shell ;;
esac

# fzf
if (( $+commands[fzf] )); then
    source ${dotHome}/zsh/fzf.zsh
fi

if [[ -d "${fzfBase}" ]]; then
    # key-bindings
    source ${fzfBase}/key-bindings.zsh

    # Auto-completion
    [[ $- == *i* ]] && source ${fzfBase}/completion.zsh 2> /dev/null
fi

# zoxide
# https://github.com/ajeetdsouza/zoxide
if (( $+commands[zoxide] )); then
    eval "$(zoxide init zsh)"
fi

# starship
#  https://github.com/starship/starship
if (( $+commands[starship] )); then
    export STARSHIP_CONFIG=${dotHome}/starship.toml
    eval "$(starship init zsh)"
fi

# Ensure exa is available
if (( $+commands[exa] )); then
    alias ls='exa --color=auto --group-directories-first --time-style=long-iso'
    alias la='ls -abghHliS'
    alias tree='ls -T'
fi

alias cat='bat'
alias diff='delta -ns'

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
