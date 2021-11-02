# zoxide
# https://github.com/ajeetdsouza/zoxide
if (( $+commands[zoxide] )); then
	eval "$(zoxide init zsh)"
fi

# starship
#  https://github.com/starship/starship
if (( $+commands[starship] )); then
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

if (( $+commands[fzf] )); then
    source $HOME/myDoc/dotfiles/zsh/fzf.zsh
fi
