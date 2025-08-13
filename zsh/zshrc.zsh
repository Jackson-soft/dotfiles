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

# Initialize completion system
autoload -Uz compinit
compinit -i

### End of Zinit's installer chunk

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode depth"1" for \
    ${ZI_REPO}/zinit-annex-bin-gem-node \
    atload"[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh" romkatv/powerlevel10k

# Completion enhancements
zinit wait"1" lucid depth"1" light-mode for \
    atclone"source fzf-tab.zsh && build-fzf-tab-module" atpull"%atclone" Aloxaf/fzf-tab \
    ${ZI_REPO}/fast-syntax-highlighting \
    atload"ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20;ZSH_AUTOSUGGEST_STRATEGY=(history completion);ZSH_AUTOSUGGEST_USE_ASYNC=1" \
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
    sbin"**/eza" if'[[ $OSTYPE != darwin* ]]' eza-community/eza \
    sbin"fzf" atload'eval "$(fzf --zsh)"' junegunn/fzf \
    sbin"**/delta" atload"alias diff='delta -ns'" dandavison/delta \
    sbin"**/fd" cp"**/fd.1 -> $ZPFX/man/man1" completions @sharkdp/fd \
    sbin"buf* -> buf" atload"source <(buf completion zsh)" bufbuild/buf \
    sbin"bin/lua-language-server" bpick"*64.tar.gz" LuaLS/lua-language-server \
    sbin"neocmakelsp* -> neocmakelsp" neocmakelsp/neocmakelsp \
    sbin"btm" atload"alias top=btm" completions ClementTsang/bottom \
    sbin"marksman* -> marksman" artempyanykh/marksman

# Modern command aliases with fallbacks
if (( $+commands[eza] )); then
    alias ls='eza --color=auto --icons --group-directories-first'
    alias ll='eza -alh --time-style=long-iso --icons --group-directories-first'
    alias la='eza -a --icons'
    alias tree='eza -T --icons'
else
    # Fallback to traditional ls with colors
    if [[ $OSTYPE == darwin* ]]; then
        alias ls='ls -G'
        alias ll='ls -alh'
    else
        alias ls='ls --color=auto'
        alias ll='ls -alh --color=auto'
    fi
    alias la='ls -a'
    (( $+commands[tree] )) && alias tree='tree' || alias tree='find . -type d | head -20'
fi

(( $+commands[bat] )) && alias cat='bat -p --wrap character'
(( $+commands[tldr] )) && alias help='tldr'

source $HOME/myDoc/dotfiles/zsh/conf.zsh

# Load local customizations if they exist
[[ -f ~/.zshrc.local ]] && source ~/.zshrc.local

# Load work-specific configurations if they exist
[[ -f ~/.zshrc.work ]] && source ~/.zshrc.work

# macOS specific configurations
if [[ "$OSTYPE" == "darwin"* ]]; then
    # Add Homebrew to PATH if it exists
    if [[ -f "/opt/homebrew/bin/brew" ]]; then
        eval "$(/opt/homebrew/bin/brew shellenv)"
    elif [[ -f "/usr/local/bin/brew" ]]; then
        eval "$(/usr/local/bin/brew shellenv)"
    fi
    
    # macOS specific aliases
    alias cleanup="find . -type f -name '*.DS_Store' -ls -delete"
fi

# Performance: Compile zshrc if it's newer than the compiled version
if [[ "$HOME/.zshrc" -nt "$HOME/.zshrc.zwc" ]] || [[ ! -s "$HOME/.zshrc.zwc" ]]; then
    zcompile "$HOME/.zshrc"
fi
