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
    sbin"starship" atload'eval "$(starship init zsh)"' starship/starship \
    sbin"fzf" src'key-bindings.zsh' dl'https://raw.githubusercontent.com/junegunn/fzf/master/shell/key-bindings.zsh' junegunn/fzf

# Completion enhancements
zinit wait lucid depth"1" light-mode nocd for \
    atinit"zicompinit; zicdreplay" \
        Aloxaf/fzf-tab \
        ${ZI_REPO}/fast-syntax-highlighting \
    atinit"ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20;ZSH_AUTOSUGGEST_STRATEGY=(history completion match_prev_cmd)" atload"_zsh_autosuggest_start" \
        zsh-users/zsh-autosuggestions \
    trackbinds bindmap"^R -> ^H" \
        ${ZI_REPO}/history-search-multi-word \
    blockf atpull'zinit creinstall -q .' \
        zsh-users/zsh-completions

# git extensions
zi ice wait"0a" lucid depth"1"
zi light wfxr/forgit

# Modern Unix commands
# See https://github.com/ibraheemdev/modern-unix
zinit wait lucid as"null" from"gh-r" for \
    sbin"**/delta" atload"alias diff='delta -ns'" dandavison/delta \
    sbin"zoxide" atload'eval "$(zoxide init zsh)"' completions ajeetdsouza/zoxide \
    sbin"**/fd" cp"**/fd.1 -> $ZPFX/man/man1" completions @sharkdp/fd \
    sbin"**/vivid" atload'export LS_COLORS="$(vivid generate catppuccin-macchiato)"' @sharkdp/vivid \
    sbin"buf* -> buf" atload"source <(buf completion zsh)" bufbuild/buf \
    sbin"**/golangci-lint" atload"source <(golangci-lint completion zsh)" golangci/golangci-lint \
    sbin"bin/lua-language-server" LuaLS/lua-language-server \
    sbin"neocmakelsp* -> neocmakelsp" Decodetalkers/neocmakelsp \
    sbin"marksman* -> marksman" artempyanykh/marksman \

if (( $+commands[eza] )); then
    alias ls='eza --color=auto --icons --group-directories-first'
    alias ll='ls -alh --time-style=long-iso'
    alias la='ls -a'
    alias tree='ls -T'
fi
(( $+commands[bat] )) && alias cat='bat -p --wrap character'
(( $+commands[btm] )) && alias top=btm
(( $+commands[procs] )) && alias ps=procs
(( $+commands[tldr] )) && alias help=tldr

source $HOME/myDoc/dotfiles/zsh/conf.zsh
