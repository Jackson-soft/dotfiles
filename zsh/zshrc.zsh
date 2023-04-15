### Added by Zinit's installer
local ZI_REPO="zdharma-continuum"
ZI_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit"
ZI_BIN="${ZI_HOME}/zinit.git"

if [[ ! -e ${ZI_BIN}/zinit.zsh ]] {
    command mkdir -p "$(dirname ${ZI_HOME})" && command git clone "https://github.com/${ZI_REPO}/zinit.git" ${ZI_BIN}
    command chmod g-rwX ${ZI_HOME} && zcompile ${ZI_BIN}/zinit.zsh
}

if [[ -e $ZI_BIN/zinit.zsh ]] {
    builtin source ${ZI_BIN}/zinit.zsh \
    && autoload -Uz _zinit \
    && (( ${+_comps} )) \
    && _comps[zinit]=_zinit
}
### End of Zinit's installer chunk

# see https://thevaluable.dev/zsh-completion-guide-examples
zinit depth"1" light-mode for \
        ${ZI_REPO}/zinit-annex-{'bin-gem-node','patch-dl'} \
    atinit"ZINIT[COMPINIT_OPTS]=-C; zpcompinit; zpcdreplay" \
        ${ZI_REPO}/fast-syntax-highlighting \
        Aloxaf/fzf-tab \
    atinit"ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20;" atload"!_zsh_autosuggest_start" \
        zsh-users/zsh-autosuggestions \
    blockf atpull'zinit creinstall -q .' \
        zsh-users/zsh-completions \
    trackbinds bindmap"^R -> ^H" \
        ${ZI_REPO}/history-search-multi-word

# Load starship theme
zinit ice as"null" from"gh-r" atclone"starship init zsh > init.zsh; starship completions zsh > _starship" atpull"%atclone" src"init.zsh" completions sbin"starship"
zinit light starship/starship

# git extensions
zinit wait"0a" lucid depth"1" for \
    as"program" pick"$ZPFX/bin/git-*" src"etc/git-extras-completion.zsh" make"PREFIX=$ZPFX" tj/git-extras \
    wfxr/forgit

# Modern Unix commands
# See https://github.com/ibraheemdev/modern-unix
zinit wait lucid as"null" from"gh-r" for \
    sbin"bin/exa" atload"alias ls='exa --color=auto --group-directories-first --time-style=long-iso';alias ll='ls -lh';alias la='ls -abghHliS';alias tree='ls -T'" \
    cp"**/exa.1 -> $ZPFX/share/man/man1" mv"completions/exa.zsh -> _exa" completions sbin"**/exa" ogham/exa \
    atload"alias cat='bat -p --wrap character'" cp"**/bat.1 -> $ZPFX/share/man/man1" mv"**/autocomplete/bat.zsh -> _bat" completions sbin"**/bat" @sharkdp/bat \
    atclone"zoxide init zsh > z.zsh" atpull"%atclone" src"z.zsh" completions sbin"zoxide" ajeetdsouza/zoxide \
    sbin"**/delta" atload"alias diff='delta -ns'" dandavison/delta \
    sbin"difft" Wilfred/difftastic \
    cp"**/doc/rg.1 -> $ZPFX/share/man/man1" completions sbin"**/rg" BurntSushi/ripgrep \
    cp"**/fd.1 -> $ZPFX/share/man/man1" completions sbin"**/fd" @sharkdp/fd \
    sbin"**/vivid" atload'export LS_COLORS="$(vivid generate one-dark)"' @sharkdp/vivid \
    sbin"jq* -> jq" stedolan/jq \
    sbin"buf* -> buf" bufbuild/buf \
    sbin"**/golangci-lint" atload"source <(golangci-lint completion zsh); compdef _golangci-lint golangci-lint" golangci/golangci-lint \
    sbin"ruff" charliermarsh/ruff \
    sbin"bin/lua-language-server" LuaLS/lua-language-server \
    sbin"hadolint* -> hadolint" hadolint/hadolint \
    sbin"**/shellcheck" koalaman/shellcheck \
    sbin"neocmakelsp* -> neocmakelsp" Decodetalkers/neocmakelsp \
    sbin"bin/pandoc" jgm/pandoc \
    atload"alias ps=procs" sbin"procs" dalance/procs \
    atload"alias top=btm" completions sbin"btm" ClementTsang/bottom \
    atload"alias help=tldr" mv"tealdeer* -> tldr" dl'https://github.com/dbrgn/tealdeer/releases/latest/download/completions_zsh -> _tldr;' completions sbin"tldr" dbrgn/tealdeer \
    sbin"marksman* -> marksman" artempyanykh/marksman \
    sbin"**/shfmt* -> shfmt" @mvdan/sh

zinit ice wait"0b" lucid as"null" from"gh-r" src"key-bindings.zsh" completions sbin"fzf" \
    dl="$(print -c https://raw.githubusercontent.com/junegunn/fzf/master/{shell/{'key-bindings.zsh;','completion.zsh -> _fzf;'},'man/man1/fzf.1 -> $ZPFX/share/man/man1/fzf.1;'})"
zinit light junegunn/fzf

source $HOME/myDoc/dotfiles/zsh/conf.zsh

export PATH=/usr/local/opt/llvm/bin:$PATH:$HOME/go/bin
