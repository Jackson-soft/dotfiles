# Enable Powerlevel7k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

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
    ${ZI_REPO}/zinit-annex-bin-gem-node \
    atload"[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh" romkatv/powerlevel10k

# Completion enhancements
zinit wait lucid depth"1" light-mode for \
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
zinit wait"0a" lucid depth"1" for \
    atload"source <(lua $ZINIT[PLUGINS_DIR]/skywind3000---z.lua/z.lua --init zsh enhanced once fzf);export _ZL_HYPHEN=1" skywind3000/z.lua \
    wfxr/forgit

# Modern Unix commands
# See https://github.com/ibraheemdev/modern-unix
zinit wait lucid as"null" from"gh-r" for \
    sbin"fzf" atload'eval "$(fzf --zsh)"'  junegunn/fzf \
    sbin"**/delta" atload"alias diff='delta -ns'" dandavison/delta \
    sbin"**/fd" cp"**/fd.1 -> $ZPFX/man/man1" completions @sharkdp/fd \
    sbin"**/vivid" atload'export LS_COLORS="$(vivid generate catppuccin-macchiato)"' @sharkdp/vivid \
    sbin"buf* -> buf" atload"source <(buf completion zsh)" bufbuild/buf \
    sbin"**/golangci-lint" atload"source <(golangci-lint completion zsh)" golangci/golangci-lint \
    sbin"bin/lua-language-server" bpick"*x64.tar.gz" LuaLS/lua-language-server \
    sbin"neocmakelsp* -> neocmakelsp" Decodetalkers/neocmakelsp \
    sbin"btm" atload"alias top=btm" completions ClementTsang/bottom \
    sbin"marksman* -> marksman" artempyanykh/marksman

if (( $+commands[eza] )); then
    alias ls='eza --color=auto --icons --group-directories-first'
    alias ll='ls -alh --time-style=long-iso'
    alias la='ls -a'
    alias tree='ls -T'
fi

(( $+commands[bat] )) && alias cat='bat -p --wrap character'
(( $+commands[tldr] )) && alias help='tldr'

source $HOME/myDoc/dotfiles/zsh/conf.zsh
