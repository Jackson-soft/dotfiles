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

zinit light-mode for \
    zdharma-continuum/zinit-annex-patch-dl \
    zdharma-continuum/zinit-annex-bin-gem-node

zinit wait lucid for \
    atinit"ZINIT[COMPINIT_OPTS]=-C; zicompinit; zicdreplay" \
        Aloxaf/fzf-tab \
        zdharma-continuum/fast-syntax-highlighting \
    blockf \
        zsh-users/zsh-completions \
    atload"!_zsh_autosuggest_start" \
        zsh-users/zsh-autosuggestions

# git extensions
zinit lucid wait'0a' as"program" for \
    pick"$ZPFX/bin/git-*" make"PREFIX=$ZPFX" src"etc/git-extras-completion.zsh" tj/git-extras

# Load starship theme
zinit ice as"null" from"gh-r" \
    sbin"starship" atclone"starship init zsh > init.zsh; starship completions zsh > _starship" atpull"%atclone" src"init.zsh"
zinit light starship/starship

# Modern Unix commands
# See https://github.com/ibraheemdev/modern-unix
zinit as"null" wait"1" lucid from"gh-r" for \
    sbin"**/delta" atload"alias diff='delta -ns'" dandavison/delta \
    sbin"stylua" JohnnyMorganz/StyLua \
    sbin"selene" Kampfkarren/selene \
    sbin"**/rg" cp"**/doc/rg.1 -> $ZINIT[MAN_DIR]/man1" mv"**/complete/_rg -> $ZINIT[COMPLETIONS_DIR]" BurntSushi/ripgrep \
    sbin"**/fd" cp"**/fd.1 -> $ZINIT[MAN_DIR]/man1" mv"**/autocomplete/_fd -> $ZINIT[COMPLETIONS_DIR]" @sharkdp/fd \
    sbin"zoxide" cp"man/zoxide.1 -> $ZINIT[MAN_DIR]/man1" atclone"zoxide init zsh > z.zsh" atpull"%atclone" src"z.zsh" nocompile'!' ajeetdsouza/zoxide \
    sbin"**/bat" atload"alias cat='bat'" cp"**/bat.1 -> $ZINIT[MAN_DIR]/man1" mv"**/autocomplete/bat.zsh -> $ZINIT[COMPLETIONS_DIR]/_bat" @sharkdp/bat \
    sbin"**/exa" atload"alias ls='exa --color=auto --group-directories-first --time-style=long-iso';alias ll='ls -lh';alias la='ls -abghHliS';alias tree='ls -T'" \
    cp"**/man/exa.1 -> $ZINIT[MAN_DIR]/man1" mv"**/completions/exa.zsh -> $ZINIT[COMPLETIONS_DIR]/_exa" ogham/exa \
    sbin"cheat" mv"cheat** -> cheat" cheat/cheat \
    sbin"procs" atload"alias ps=procs" dalance/procs \
    sbin"jq" mv"jq* -> jq" stedolan/jq \
    sbin"buf" mv"buf* -> buf" bufbuild/buf \
    sbin"hadolint" mv"hadolint* -> hadolint" hadolint/hadolint \
    sbin"**/shellcheck" koalaman/shellcheck \
    sbin"shfmt" mv"shfmt* -> shfmt" @mvdan/sh

zinit ice as"null" wait lucid from"gh-r" sbin"fzf" \
    dl'https://raw.githubusercontent.com/junegunn/fzf/master/shell/completion.zsh -> _fzf' \
    dl'https://raw.githubusercontent.com/junegunn/fzf/master/shell/key-bindings.zsh -> key-bindings.zsh' \
    dl'https://raw.githubusercontent.com/junegunn/fzf/master/man/man1/fzf.1 -> fzf.1' \
    src'key-bindings.zsh' atclone"cp -vf fzf.1 $ZINIT[MAN_DIR]/man1; cp -vf _fzf $ZINIT[COMPLETIONS_DIR]"
zinit light junegunn/fzf

# completion
zinit ice as"completion"
zinit snippet https://github.com/docker/cli/blob/master/contrib/completion/zsh/_docker

zinit light-mode lucid wait has"kubectl" for \
    id-as"kubectl-completion" \
    as"completion" \
    atclone"kubectl completion zsh > _kubectl" \
    atpull"%atclone" \
    zdharma-continuum/null

local dotHome=${HOME}/myDoc/dotfiles
source ${dotHome}/zsh/conf.zsh
# fzf
if (( $+commands[fzf] )); then
    source ${dotHome}/zsh/fzf.zsh
fi

export PATH=/usr/local/opt/llvm/bin:$PATH:$HOME/go/bin

