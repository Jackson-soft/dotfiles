### Added by Zinit's installer
local ZI_REPO="zdharma-continuum"
ZI_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit"
ZI_BIN="${ZI_HOME}/zinit.git"

if [[ ! -e ${ZI_BIN}/zinit.zsh ]] {
    command mkdir -p "$(dirname ${ZI_HOME})" && command git clone --depth=1 "https://github.com/${ZI_REPO}/zinit.git" ${ZI_BIN}
    command chmod g-rwX ${ZI_HOME} && zcompile ${ZI_BIN}/zinit.zsh
}

source ${ZI_BIN}/zinit.zsh

autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit
### End of Zinit's installer chunk

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode depth"1" for \
    ${ZI_REPO}/zinit-annex-{'patch-dl','bin-gem-node'}

zinit as"null" from"gh-r" light-mode for \
    sbin"starship" atload"eval '$(starship init zsh)'" starship/starship \
    sbin"fzf" src'key-bindings.zsh' compile'key-bindings.zsh' completions \
    dl="$(print -c https://raw.githubusercontent.com/junegunn/fzf/master/{shell/{'key-bindings.zsh;','completion.zsh -> _fzf;'},'man/man1/fzf.1 -> $ZPFX/man/man1/fzf.1;'})" junegunn/fzf

# Completion enhancements
zinit wait lucid depth"1" light-mode nocd for \
    atinit"zicompinit; zicdreplay" \
        Aloxaf/fzf-tab \
        ${ZI_REPO}/fast-syntax-highlighting \
    atinit"ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20;" atload"_zsh_autosuggest_start" \
        zsh-users/zsh-autosuggestions \
    trackbinds bindmap"^R -> ^H" \
        ${ZI_REPO}/history-search-multi-word \
    blockf atpull'zinit creinstall -q .' \
        zsh-users/zsh-completions

# git extensions
zinit wait"0a" lucid depth"1" for \
    as"program" pick"$ZPFX/bin/git-*" src"etc/git-extras-completion.zsh" make"PREFIX=$ZPFX" tj/git-extras \
    atload"source <(lua $ZINIT[PLUGINS_DIR]/skywind3000---z.lua/z.lua --init zsh enhanced once fzf);export _ZL_HYPHEN=1" skywind3000/z.lua \
    wfxr/forgit

# Modern Unix commands
# See https://github.com/ibraheemdev/modern-unix
zinit wait lucid as"null" from"gh-r" for \
    sbin"**/lsd" atload"alias ls='lsd';alias ll='ls -Alh';alias la='ls -A';alias tree='ls --tree'" cp"**/lsd.1 -> $ZPFX/man/man1" completions lsd-rs/lsd \
    sbin"**/bat" atload"alias cat='bat -p --wrap character'" cp"**/bat.1 -> $ZPFX/man/man1" mv"**/autocomplete/bat.zsh -> _bat" completions @sharkdp/bat \
    sbin"**/delta" atload"alias diff='delta -ns'" dandavison/delta \
    sbin"**/rg" cp"**/doc/rg.1 -> $ZPFX/man/man1" completions BurntSushi/ripgrep \
    sbin"**/fd" cp"**/fd.1 -> $ZPFX/man/man1" completions @sharkdp/fd \
    sbin"**/vivid" atload'export LS_COLORS="$(vivid generate catppuccin-macchiato)"' @sharkdp/vivid \
    sbin"jq* -> jq" jqlang/jq \
    sbin"buf* -> buf" atload"source <(buf completion zsh)" bufbuild/buf \
    sbin"**/golangci-lint" atload"source <(golangci-lint completion zsh)" golangci/golangci-lint \
    sbin"ruff" @astral-sh/ruff \
    sbin"bin/lua-language-server" LuaLS/lua-language-server \
    sbin"hadolint* -> hadolint" hadolint/hadolint \
    sbin"**/shellcheck" koalaman/shellcheck \
    sbin"neocmakelsp* -> neocmakelsp" Decodetalkers/neocmakelsp \
    sbin"bin/pandoc" jgm/pandoc \
    sbin"procs" atload"alias ps=procs" dalance/procs \
    sbin"btm" atload"alias top=btm" completions ClementTsang/bottom \
    sbin"tldr" atload"alias help=tldr" mv"tealdeer* -> tldr" dl'https://github.com/dbrgn/tealdeer/releases/latest/download/completions_zsh -> _tldr;' completions dbrgn/tealdeer \
    sbin"marksman* -> marksman" artempyanykh/marksman \
    sbin"**/shfmt* -> shfmt" @mvdan/sh

source $HOME/myDoc/dotfiles/zsh/conf.zsh
