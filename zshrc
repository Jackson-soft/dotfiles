# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

### Added by Zinit's installer
if [[ ! -f $HOME/.zinit/bin/zinit.zsh ]]; then
    print -P "%F{33}▓▒░ %F{220}Installing %F{33}DHARMA%F{220} Initiative Plugin Manager (%F{33}zdharma/zinit%F{220})…%f"
    command mkdir -p "$HOME/.zinit" && command chmod g-rwX "$HOME/.zinit"
    command git clone https://github.com/zdharma/zinit "$HOME/.zinit/bin" && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
        print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi

# Changing/making/removing directory
setopt auto_pushd
setopt auto_cd
setopt pushd_ignore_dups
setopt pushdminus

source "$HOME/.zinit/bin/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zinit-zsh/z-a-bin-gem-node \
    zinit-zsh/z-a-patch-dl \
    zinit-zsh/z-a-readurl

# Fast-syntax-highlighting & autosuggestions
zinit wait lucid for \
    atinit"ZINIT[COMPINIT_OPTS]=-C; zpcompinit; zpcdreplay" \
        zdharma/fast-syntax-highlighting \
    atload"!_zsh_autosuggest_start" \
        zsh-users/zsh-autosuggestions \
    blockf \
        zsh-users/zsh-completions

zinit ice depth=1; zinit light romkatv/powerlevel10k

# A few wait"1 plugins
zinit wait"1" lucid for \
    atinit'zstyle ":history-search-multi-word" page-size "7"' \
        zdharma/history-search-multi-word \
    agkozak/zsh-z \
    wfxr/forgit

# A few wait'2' git extensions
zinit as"null" wait"2" lucid for \
    src"etc/git-extras-completion.zsh" make"PREFIX=$ZPFX" tj/git-extras

zinit as"null" wait lucid from"gh-r" for \
    cp"**/man/exa.1 -> $ZPFX/share/man/man1/" mv"**/completions/exa.zsh -> $ZINIT[COMPLETIONS_DIR]/_exa" sbin"**/exa" ogham/exa \
    cp"**/bat.1 -> $ZPFX/share/man/man1/" mv"**/autocomplete/bat.zsh -> $ZINIT[COMPLETIONS_DIR]/_bat" sbin"**/bat" @sharkdp/bat \
    cp"**/fd.1 -> $ZPFX/share/man/man1/" mv"**/autocomplete/_fd -> $ZINIT[COMPLETIONS_DIR]/_fd" sbin"**/fd" @sharkdp/fd \
    cp"**/doc/rg.1 -> $ZPFX/share/man/man1/" mv"**/complete/_rg -> $ZINIT[COMPLETIONS_DIR]/_rg" sbin"**/rg" BurntSushi/ripgrep \
    atload"alias ps=procs" sbin"**/procs" dalance/procs \
    sbin"**/delta" dandavison/delta \
    mv"shfmt* -> shfmt" sbin"shfmt" @mvdan/sh

zinit ice as"null" wait lucid from"gh-r" sbin"fzf" \
    dl'https://raw.githubusercontent.com/junegunn/fzf/master/shell/completion.zsh -> _fzf_completion; https://raw.githubusercontent.com/junegunn/fzf/master/shell/key-bindings.zsh -> key-bindings.zsh; https://raw.githubusercontent.com/junegunn/fzf/master/man/man1/fzf.1 -> $ZPFX/share/man/man1/fzf.1' \
    src'key-bindings.zsh'
zinit light junegunn/fzf

zinit ice from"gh-r" sbin"*/vivid"
zinit light sharkdp/vivid

export LS_COLORS="$(vivid generate molokai)"

zinit ice as"completion"
zinit snippet https://github.com/docker/cli/blob/master/contrib/completion/zsh/_docker

zinit ice wait lucid atload"zicompinit; zicdreplay" blockf
zinit light Aloxaf/fzf-tab

zstyle ':completion:*:descriptions' format '[%d]'
zstyle ':completion:*:git-checkout:*' sort false
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:*:*:*:processes' command 'ps -u $USER -o pid,user,comm -w -w'
zstyle ':fzf-tab:*' switch-group ',' '.'
zstyle ':fzf-tab:complete:(cd|exa|ls|bat|cat|emacs|vim):*' fzf-preview 'exa -1 --color=always $realpath'
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-preview \
       '[[ $group == "[process ID]" ]] &&
        if [[ $OSTYPE == darwin* ]]; then
           ps -p $word -o comm="" -w -w
        elif [[ $OSTYPE == linux* ]]; then
           ps --pid=$word -o cmd --no-headers -w -w
        fi'
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-flags '--preview-window=down:3:wrap'

alias -g ...='../..'
alias ls='exa --color=auto --group-directories-first --time-style=long-iso'
alias ll='ls -lh'
alias la='ls -laFh'
alias tree='ls -T'

export FZF_DEFAULT_COMMAND="fd --type f --hidden --follow --exclude .git || git ls-tree -r --name-only HEAD || rg --hidden --files || find ."
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_CTRL_T_OPTS="--preview '(bat --style=numbers --color=always {} || cat {} || exa -T {}) 2> /dev/null | head -200'"
export FZF_CTRL_R_OPTS="--preview 'echo {}' --preview-window down:3:hidden:wrap --bind '?:toggle-preview' --exact"
export FZF_ALT_C_OPTS="--preview 'exa -T {} | head -200'"
export FZF_DEFAULT_OPTS="--height=40% --exact --cycle --layout=reverse --info=inline --border --margin=1 --padding=1"

### End of Zinit's installer chunk

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
