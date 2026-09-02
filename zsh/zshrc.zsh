### Added by Zinit's installer
typeset -g ZI_REPO="zdharma-continuum"
ZI_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit"
ZI_BIN="${ZI_HOME}/zinit.git"

if [[ ! -e "${ZI_BIN}/zinit.zsh" ]] {
    command mkdir -p "$(dirname "${ZI_HOME}")" && command git clone --depth=1 "https://github.com/${ZI_REPO}/zinit.git" "${ZI_BIN}"
    command chmod g-rwX "${ZI_HOME}" && zcompile "${ZI_BIN}/zinit.zsh"
}

# Performance tuning (before sourcing zinit)
declare -A ZINIT
ZINIT[OPTIMIZE_OUT_DISK_ACCESSES]=1
ZINIT[COMPINIT_OPTS]="-C"

source "${ZI_BIN}/zinit.zsh"

autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

### End of Zinit's installer chunk

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode depth"1" for \
    ${ZI_REPO}/zinit-annex-bin-gem-node

# Completion enhancements (wait"0" = load at first prompt for fast tab-completion)
# Order: completions → compinit → fzf-tab (needs compinit) → autosuggestions → syntax-highlighting (last per docs)
zinit lucid depth"1" light-mode for \
    blockf atpull'zinit creinstall -q .' \
        zsh-users/zsh-completions \
    atinit"zicompinit; zicdreplay" atclone"source fzf-tab.zsh && build-fzf-tab-module" atpull"%atclone" \
        Aloxaf/fzf-tab \
    atload"ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20;ZSH_AUTOSUGGEST_STRATEGY=(history completion)" \
        zsh-users/zsh-autosuggestions \
    ${ZI_REPO}/fast-syntax-highlighting

# git extensions (deferred – less urgent than completions)
zinit wait"0a" lucid light-mode for \
    as"program" pick"bin/git-*" src"etc/git-extras-completion.zsh" tj/git-extras \
    wfxr/forgit

# Modern Unix commands
# See https://github.com/ibraheemdev/modern-unix
# LSP / CLI tools
zinit wait"0a" lucid from"gh-r" as"program" for \
    sbin"**/delta" \
        atload"alias diff='delta -ns'" \
        dandavison/delta \
    sbin"buf* -> buf" \
        extract"" \
        atload"source <(buf completion zsh)" \
        bufbuild/buf \
    sbin"**/lua-language-server" \
        LuaLS/lua-language-server \
    sbin"**/neocmakelsp" \
        neocmakelsp/neocmakelsp \
    sbin"**/btm" \
        atload"alias top=btm" \
        completions \
        ClementTsang/bottom \
    sbin"marksman* -> marksman" \
        extract"" \
        artempyanykh/marksman

# Tool integrations (aliases live in conf.zsh)
(( $+commands[fzf] )) && source <(fzf --zsh)
# --cmd j: default `zi` clashes with zinit's own `zi` alias
(( $+commands[zoxide] )) && eval "$(zoxide init zsh --cmd j)"
(( $+commands[starship] )) && eval "$(starship init zsh)"

source "${${(%):-%x}:A:h}/conf.zsh"

# Eat (Emacs terminal emulator) shell integration
[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
  source "$EAT_SHELL_INTEGRATION_DIR/zsh"
