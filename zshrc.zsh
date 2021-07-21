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
setopt auto_menu         # show completion menu on successive tab press
setopt complete_in_word
setopt promptsubst

# emacs key bindings
# bindkey -e

alias -g ...='../..'

source "$HOME/.zinit/bin/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zinit-zsh/z-a-patch-dl \
    zinit-zsh/z-a-bin-gem-node \
    zinit-zsh/z-a-readurl

# Fast-syntax-highlighting & autosuggestions
zinit wait lucid light-mode for \
    atinit"zpcompinit; zpcdreplay" \
        zdharma/fast-syntax-highlighting \
    atload"_zsh_autosuggest_start" \
        zsh-users/zsh-autosuggestions \
    blockf atpull'zinit creinstall -q .' \
        zsh-users/zsh-completions

# zinit ice depth=1 atload'!source $HOME/.p10k.zsh' nocd lucid
# zinit light romkatv/powerlevel10k

# Load starship theme
zinit ice as"null" wait lucid from"gh-r" sbin"starship" atclone"starship init zsh > init.zsh; starship completions zsh > _starship" \
    atpull"%atclone" src"init.zsh"
zinit light @starship/starship

# A few wait"1 plugins
zinit wait"1" lucid for \
    atinit'
        zstyle :history-search-multi-word page-size 10
        zstyle :history-search-multi-word highlight-color fg=red,bold
        zstyle :plugin:history-search-multi-word reset-prompt-protect 1
        '\
    zdharma/history-search-multi-word

# A few wait'2' git extensions
zinit as"null" wait"2" lucid for \
    src"etc/git-extras-completion.zsh" make"PREFIX=$ZPFX" tj/git-extras \
    sbin"bin/git-fuzzy" bigH/git-fuzzy

# Modern Unix commands
# See https://github.com/ibraheemdev/modern-unix
zinit as"null" wait lucid from"gh-r" for \
    atload"alias ls='exa --color=auto --group-directories-first --time-style=long-iso';alias ll='ls -lh';alias la='ls -laFh';alias tree='ls -T'" \
    cp"**/man/exa.1 -> $ZPFX/share/man/man1/" mv"**/completions/exa.zsh -> $ZINIT[COMPLETIONS_DIR]/_exa" sbin"**/exa" ogham/exa \
    atload"alias cat='bat'" cp"**/bat.1 -> $ZPFX/share/man/man1/" mv"**/autocomplete/bat.zsh -> $ZINIT[COMPLETIONS_DIR]/_bat" sbin"**/bat" @sharkdp/bat \
    cp"**/fd.1 -> $ZPFX/share/man/man1/" mv"**/autocomplete/_fd -> $ZINIT[COMPLETIONS_DIR]" sbin"**/fd" @sharkdp/fd \
    atload'export LS_COLORS="$(vivid generate molokai)"' sbin"**/vivid" @sharkdp/vivid \
    cp"**/doc/rg.1 -> $ZPFX/share/man/man1/" mv"**/complete/_rg -> $ZINIT[COMPLETIONS_DIR]" sbin"**/rg" BurntSushi/ripgrep \
    atload"alias top=btm --color gruvbox" mv"**/completion/_btm -> $ZINIT[COMPLETIONS_DIR]" sbin"**/btm" ClementTsang/bottom \
    atload"alias ps=procs" sbin"**/procs" dalance/procs \
    sbin"stylua" JohnnyMorganz/StyLua \
    atload"alias diff='delta -n'" sbin"**/delta" dandavison/delta \
    mv"**/man/zoxide.1 -> $ZPFX/share/man/man1/" sbin"**/zoxide" atclone"zoxide init zsh > z.zsh" atpull"%atclone" src"z.zsh" nocompile'!' ajeetdsouza/zoxide \
    mv"shfmt* -> shfmt" sbin"shfmt" @mvdan/sh

zinit ice as"null" wait lucid from"gh-r" sbin"fzf" \
    dl'https://raw.githubusercontent.com/junegunn/fzf/master/shell/completion.zsh -> _fzf_completion' \
    dl'https://raw.githubusercontent.com/junegunn/fzf/master/shell/key-bindings.zsh -> key-bindings.zsh' \
    dl'https://raw.githubusercontent.com/junegunn/fzf/master/man/man1/fzf.1 -> $ZPFX/share/man/man1/fzf.1' \
    src'key-bindings.zsh'
zinit light junegunn/fzf

zinit ice as"completion"
zinit snippet https://github.com/docker/cli/blob/master/contrib/completion/zsh/_docker

zinit ice mv="*.zsh -> _fzf" as="completion"
zinit snippet 'https://github.com/junegunn/fzf/blob/master/shell/completion.zsh'

zinit ice wait lucid atload"zicompinit; zicdreplay" blockf
zinit light Aloxaf/fzf-tab

# Use caching so that commands like apt and dpkg complete are useable
zstyle ':completion:*' use-cache yes
zstyle ':completion:*' cache-path $ZSH_CACHE_DIR
# cd 不区分大小写
zstyle ':completion:*' matcher-list 'm:{a-zA-Z-_}={A-Za-z_-}' 'r:|=*' 'l:|=* r:|=*'

zstyle ':completion:*:descriptions' format '[%d]'
zstyle ':completion:*:git-checkout:*' sort false
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:*:*:*:processes' command 'ps -u $USER -o pid,user,comm -w -w'
zstyle ':fzf-tab:*' switch-group ',' '.'
zstyle ':fzf-tab:complete:(cd|exa|ls|bat|cat|emacs|nvim|vim):*' fzf-preview 'exa -1 --color=always $realpath'
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-preview \
       '[[ $group == "[process ID]" ]] &&
        if [[ $OSTYPE == darwin* ]]; then
           ps -p $word -o comm="" -w -w
        elif [[ $OSTYPE == linux* ]]; then
           ps --pid=$word -o cmd --no-headers -w -w
        fi'
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-flags '--preview-window=down:3:wrap'

export FZF_DEFAULT_COMMAND="fd --type f --hidden --follow --exclude .git || git ls-tree -r --name-only HEAD || rg --hidden --files || find ."
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_CTRL_T_OPTS="--preview '(bat --style=numbers --color=always {} || cat {} || exa -T {}) 2> /dev/null | head -200'"
export FZF_CTRL_R_OPTS="--preview 'echo {}' --preview-window down:3:hidden:wrap --bind '?:toggle-preview' --exact"
export FZF_ALT_C_OPTS="--preview 'exa -T {} | head -200'"
export FZF_DEFAULT_OPTS="--height=40% --exact --cycle --layout=reverse --info=inline --border --margin=1 --padding=1"

### End of Zinit's installer chunk

# colored-man-pages
function man() {
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

alias luamake=/Users/jacksoncy/myDoc/lua-language-server/3rd/luamake/luamake
