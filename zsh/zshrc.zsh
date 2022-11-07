### Added by Zinit's installer
local ZI_REPO="zdharma-continuum"
ZI_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit"
ZI_BIN="$ZI_HOME/zinit.git"

if [[ ! -e $ZI_BIN/zinit.zsh ]] {
    command mkdir -p "$(dirname $ZI_HOME)" && command git clone "https://github.com/$ZI_REPO/zinit.git" $ZI_BIN
    command chmod g-rwX $ZI_HOME && zcompile $ZI_BIN/zinit.zsh
}

if [[ -e $ZI_BIN/zinit.zsh ]] {
    builtin source $ZI_BIN/zinit.zsh \
    && autoload -Uz _zinit \
    && (( ${+_comps} )) \
    && _comps[zinit]=_zinit
}
### End of Zinit's installer chunk

# see https://thevaluable.dev/zsh-completion-guide-examples
zinit depth"1" light-mode for \
        $ZI_REPO/zinit-annex-{'bin-gem-node','patch-dl'} \
    atinit"ZINIT[COMPINIT_OPTS]=-C; zpcompinit; zpcdreplay" \
        $ZI_REPO/fast-syntax-highlighting \
    atinit'
        zstyle ":fzf-tab:complete:(z|cd|exa):*" fzf-preview "exa -1 --color=always $realpath"
        zstyle ":fzf-tab:complete:(\\|*/|)man:*" fzf-preview "man $word"
        zstyle ":fzf-tab:complete:git-(add|diff|restore):*" fzf-preview "git diff $word | delta"
        zstyle ":fzf-tab:complete:git-log:*" fzf-preview "git log --color=always $word"
        zstyle ":fzf-tab:*" switch-group "," "."
    ' \
        Aloxaf/fzf-tab \
    atinit"ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20;" atload"!_zsh_autosuggest_start" \
        zsh-users/zsh-autosuggestions \
    blockf atpull'zinit creinstall -q .' \
    atinit'
        zstyle ":completion:*" completer _expand_alias _expand _complete _ignored _approximate
        zstyle ":completion:*" matcher-list "m:{a-zA-Z}={A-Za-z}" "+r:|?=**"
        zstyle ":completion:*" menu select=2
        zstyle ":completion:*" special-dirs true
        zstyle ":completion:*" list-colors ${(s.:.)LS_COLORS}
        zstyle ":completion:*" rehash true
        zstyle ":completion:*:*:kill:*:processes" list-colors "=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01"
        zstyle ":completion:*" select-prompt "%SScrolling active: current selection at %p%s"
        zstyle ":completion:*:descriptions" format "[%d]"
        zstyle ":completion:*:processes" command "ps -au $USER"
        zstyle ":completion:*:*:*:*:processes" command "ps -u $USER -o pid,user,comm,cmd -w -w"
        zstyle ":completion:*" file-sort modification
        zstyle ":completion:*:git-checkout:*" sort false
        zstyle ":completion:*" verbose yes
        zstyle ":completion:*" squeeze-slashes true
        zstyle ":completion:*" accept-exact "*(N)"
        zstyle ":completion:*" use-cache on
        zstyle ":completion:*" cache-path $ZSH_CACHE_DIR
    ' \
        zsh-users/zsh-completions \
    trackbinds bindmap"^R -> ^H" atinit'
        zstyle ":history-search-multi-word" page-size "20"
        zstyle ":history-search-multi-word" highlight-color "fg=red,bold"
        zstyle ":plugin:history-search-multi-word" reset-prompt-protect "1"
    ' \
        $ZI_REPO/history-search-multi-word

# Load starship theme
zinit ice as"null" from"gh-r" sbin"starship" atclone"starship init zsh > init.zsh; starship completions zsh > _starship" atpull"%atclone" src"init.zsh"
zinit light starship/starship

# git extensions
zinit wait"0a" lucid depth"1" for \
    as"program" pick"$ZPFX/bin/git-*" src"etc/git-extras-completion.zsh" make"PREFIX=$ZPFX" tj/git-extras \
    wfxr/forgit

local GH_RAW_URL="https://raw.githubusercontent.com"

# Modern Unix commands
# See https://github.com/ibraheemdev/modern-unix
zinit wait lucid as"null" from"gh-r" for \
    sbin"**/exa" atload"alias ls='exa --color=auto --group-directories-first --time-style=long-iso';alias ll='ls -lh';alias la='ls -abghHliS';alias tree='ls -T'" \
    mv"**/exa.1 -> $ZPFX/share/man/man1" cp"**/exa.zsh -> $ZINIT[COMPLETIONS_DIR]/_exa" ogham/exa \
    sbin"**/bat" atload"alias cat='bat'" mv"**/bat.1 -> $ZPFX/share/man/man1" cp"**/autocomplete/bat.zsh -> $ZINIT[COMPLETIONS_DIR]/_bat" @sharkdp/bat \
    sbin"zoxide" atclone"zoxide init zsh > z.zsh" atpull"%atclone" src"z.zsh" ajeetdsouza/zoxide \
    sbin"**/delta" atload"alias diff='delta -ns'" dandavison/delta \
    sbin"**/rg" mv"**/doc/rg.1 -> $ZPFX/share/man/man1" cp"**/complete/_rg -> $ZINIT[COMPLETIONS_DIR]" BurntSushi/ripgrep \
    sbin"**/fd" mv"**/fd.1 -> $ZPFX/share/man/man1" cp"**/autocomplete/_fd -> $ZINIT[COMPLETIONS_DIR]" @sharkdp/fd \
    sbin"**/vivid" atload'export LS_COLORS="$(vivid generate one-dark)"' @sharkdp/vivid \
    sbin"cheat* -> cheat" cheat/cheat \
    sbin"jq* -> jq" stedolan/jq \
    sbin"buf* -> buf" bufbuild/buf \
    sbin"**/golangci-lint" atload"source <(golangci-lint completion zsh); compdef _golangci-lint golangci-lint" golangci/golangci-lint \
    sbin"bin/lua-language-server" sumneko/lua-language-server \
    sbin"hadolint* -> hadolint" hadolint/hadolint \
    sbin"**/shellcheck" koalaman/shellcheck \
    sbin"**/shfmt* -> shfmt" @mvdan/sh

zinit ice wait"0b" lucid as"null" from"gh-r" src"key-bindings.zsh" sbin"fzf" \
    dl"${GH_RAW_URL}/junegunn/fzf/master/shell/key-bindings.zsh;
       ${GH_RAW_URL}/junegunn/fzf/master/shell/completion.zsh -> $ZPFX/completions/_fzf_completion;
       ${GH_RAW_URL}/junegunn/fzf/master/man/man1/fzf.1 -> $ZPFX/share/man/man1/fzf.1"
zinit light junegunn/fzf

source $HOME/myDoc/dotfiles/zsh/conf.zsh

export PATH=/usr/local/opt/llvm/bin:$PATH:$HOME/go/bin
