### Added by Zinit's installer
if [[ ! -f $HOME/.local/share/zinit/zinit.git/zinit.zsh ]]; then
    print -P "%F{33} %F{220}Installing %F{33}ZDHARMA-CONTINUUM%F{220} Initiative Plugin Manager (%F{33}zdharma-continuum/zinit%F{220})…%f"
    command mkdir -p "$HOME/.local/share/zinit" && command chmod g-rwX "$HOME/.local/share/zinit"
    command git clone https://github.com/zdharma-continuum/zinit "$HOME/.local/share/zinit/zinit.git" && \
        print -P "%F{33} %F{34}Installation successful.%f%b" || \
        print -P "%F{160} The clone has failed.%f%b"
fi

source "$HOME/.local/share/zinit/zinit.git/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit
### End of Zinit's installer chunk

ZI_REPO="zdharma-continuum"
zinit light-mode for \
    "$ZI_REPO"/zinit-annex-{'patch-dl','bin-gem-node'}

# see https://thevaluable.dev/zsh-completion-guide-examples
zinit wait lucid light-mode for \
    blockf \
    atinit"
        zstyle ':completion:*' completer _expand _complete _ignored _approximate
        zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' '+r:|?=**'
        zstyle ':completion:*' menu select=2
        zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
        zstyle ':completion:*:descriptions' format '[%d]'
        zstyle ':completion:*:processes' command 'ps -au \$USER'
        zstyle ':completion:*:*:*:*:processes' command 'ps -u \$USER -o pid,user,comm,cmd -w -w'
        zstyle ':completion:*' file-sort modification
        zstyle ':completion:*:git-checkout:*' sort false
        zstyle ':completion:*' verbose yes
        zstyle ':completion:*' squeeze-slashes true
        zstyle ':completion:*' list-colors \${(s.:.)LS_COLORS}
    " \
        zsh-users/zsh-completions \
    atinit"
        zstyle ':fzf-tab:complete:(z|cd|exa):*' fzf-preview 'exa -1 --color=always \$realpath'
        zstyle ':fzf-tab:complete:(\\|*/|)man:*' fzf-preview 'man \$word'
        zstyle ':fzf-tab:complete:git-(add|diff|restore):*' fzf-preview 'git diff \$word | delta'
        zstyle ':fzf-tab:complete:git-log:*' fzf-preview 'git log --color=always \$word'
        zstyle ':fzf-tab:*' switch-group ',' '.'
    " \
        Aloxaf/fzf-tab \
    atinit"ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20;" atload"_zsh_autosuggest_start" \
        zsh-users/zsh-autosuggestions \
    atinit"typeset -gA FAST_HIGHLIGHT; FAST_HIGHLIGHT[git-cmsg-len]=100; zicompinit; zicdreplay" \
        "$ZI_REPO"/fast-syntax-highlighting \
    trackbinds bindmap"^R -> ^H" atinit"
        zstyle :history-search-multi-word page-size 20
        zstyle :history-search-multi-word highlight-color fg=red,bold
        zstyle :plugin:history-search-multi-word reset-prompt-protect 1
    " \
        "$ZI_REPO"/history-search-multi-word

# git extensions
zinit lucid wait'0a' as"program" for \
    pick"$ZPFX/bin/git-*" make"PREFIX=$ZPFX" src"etc/git-extras-completion.zsh" tj/git-extras

# Load starship theme
zinit ice as"null" from"gh-r" \
    sbin"starship" atclone"starship init zsh > init.zsh; starship completions zsh > _starship" atpull"%atclone" src"init.zsh"
zinit light starship/starship

zinit ice depth=1 wait"1" lucid
zinit light wfxr/forgit

# Modern Unix commands
# See https://github.com/ibraheemdev/modern-unix
zinit as"null" wait"1" lucid from"gh-r" for \
    sbin"**/delta" atload"alias diff='delta -ns'" dandavison/delta \
    sbin"stylua" JohnnyMorganz/StyLua \
    sbin"selene" Kampfkarren/selene \
    sbin"**/rg" mv"**/complete/_rg -> $ZINIT[COMPLETIONS_DIR]" BurntSushi/ripgrep \
    sbin"**/fd" mv"**/autocomplete/_fd -> $ZINIT[COMPLETIONS_DIR]" @sharkdp/fd \
    sbin"**/bat" atload"alias cat='bat'" mv"**/autocomplete/bat.zsh -> $ZINIT[COMPLETIONS_DIR]/_bat" @sharkdp/bat \
    sbin"**/vivid" atload'export LS_COLORS="$(vivid generate one-dark)"' @sharkdp/vivid \
    sbin"zoxide" atclone"zoxide init zsh > z.zsh" atpull"%atclone" src"z.zsh" nocompile'!' ajeetdsouza/zoxide \
    sbin"**/exa" atload"alias ls='exa --color=auto --group-directories-first --time-style=long-iso';alias ll='ls -lh';alias la='ls -abghHliS';alias tree='ls -T'" \
    mv"**/completions/exa.zsh -> $ZINIT[COMPLETIONS_DIR]/_exa" ogham/exa \
    sbin"cheat" mv"cheat** -> cheat" cheat/cheat \
    sbin"procs" atload"alias ps=procs" dalance/procs \
    sbin"jq" mv"jq* -> jq" stedolan/jq \
    sbin"buf" mv"buf* -> buf" bufbuild/buf \
    sbin"hadolint" mv"hadolint* -> hadolint" hadolint/hadolint \
    sbin"**/shellcheck" koalaman/shellcheck \
    sbin"shfmt" mv"shfmt* -> shfmt" @mvdan/sh

zinit ice as"null" wait"0b" lucid from"gh-r" sbin"fzf" \
    dl'https://raw.githubusercontent.com/junegunn/fzf/master/shell/completion.zsh -> _fzf' \
    dl'https://raw.githubusercontent.com/junegunn/fzf/master/shell/key-bindings.zsh -> key-bindings.zsh' \
    src'key-bindings.zsh' atclone"cp -vf _fzf $ZINIT[COMPLETIONS_DIR]"
zinit light junegunn/fzf

# completion
zinit ice as"completion"
zinit snippet https://github.com/docker/cli/blob/master/contrib/completion/zsh/_docker

zinit light-mode lucid wait has"kubectl" for \
    id-as"kubectl-completion" \
    as"completion" \
    atclone"kubectl completion zsh > _kubectl" \
    atpull"%atclone" \
    "$ZI_REPO"/null

local dotHome=${HOME}/myDoc/dotfiles
source ${dotHome}/zsh/conf.zsh
# fzf
source ${dotHome}/zsh/fzf.zsh

export PATH=/usr/local/opt/llvm/bin:$PATH:$HOME/go/bin
