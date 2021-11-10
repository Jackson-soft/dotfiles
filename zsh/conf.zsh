archi=$(uname -s)
fzfBase=
case "$archi" in
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
    # key-bindings
    source ${fzfBase}/key-bindings.zsh
    source ${fzfBase}/completion.zsh 2> /dev/null
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
if (( ${+commands[exa]} )); then
    export EXA_COLORS='da=1;34:gm=1;34'
    alias ls='exa --color=auto --group-directories-first --time-style=long-iso'
    alias la='ls -laFh'
    alias tree='ls -T'
fi

alias cat='bat'
alias diff='delta -ns'

alias -g ...='../..'
