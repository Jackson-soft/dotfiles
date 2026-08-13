## Options section
setopt EXTENDED_GLOB
setopt HIST_FCNTL_LOCK      # Use modern file-locking mechanisms, for better safety & performance.
setopt HIST_IGNORE_ALL_DUPS # Keep only the most recent copy of each duplicate entry in history.
setopt HIST_IGNORE_SPACE    # Don't record commands starting with space
setopt HIST_VERIFY          # Show command with history expansion to user before running it
setopt COMPLETE_IN_WORD     # Complete from both ends of a word.
setopt ALWAYS_TO_END        # Move cursor to the end of a completed word.
setopt PATH_DIRS            # Perform path search even on command names with slashes.
setopt AUTO_LIST            # Automatically list choices on ambiguous completion.
setopt AUTO_PARAM_SLASH     # If completed parameter is a directory, add a trailing slash.
setopt CORRECT              # Try to correct the spelling of commands.
CORRECT_IGNORE="[_]*"       # Don't correct words beginning with an underscore.
setopt GLOB_DOTS            # Include dotfiles in globbing.
setopt MARK_DIRS            # Mark directories with trailing slash in filename completion.

# History settings
HISTSIZE=100000
SAVEHIST=100000
HISTFILE="${ZDOTDIR:-$HOME}/.zsh_history"
setopt SHARE_HISTORY          # Share history between sessions (implies INC_APPEND_HISTORY)
setopt EXTENDED_HISTORY       # Record timestamps and durations in history.
setopt HIST_REDUCE_BLANKS     # Remove superfluous blanks from each command
setopt HIST_EXPIRE_DUPS_FIRST # Expire duplicates first when trimming history

# Set editor default keymap to emacs (`-e`) or vi (`-v`)
bindkey -e

alias -g ...='../..'
alias -g ....='../../..'

# fzf
# https://github.com/junegunn/fzf

# NOTE: FZF_DEFAULT_COMMAND is NOT used by shell integration (CTRL-T/R/ALT-C).
# It only applies when fzf is invoked directly without stdin pipe.
(($+commands[fd])) && export FZF_DEFAULT_COMMAND="fd -t f -H -L -E .git -E node_modules -E target -E .venv -E __pycache__ --strip-cwd-prefix"

# Gruvbox color palette (single source of truth for fzf and fzf-tab)
typeset -g _fzf_palette='fg:#ebdbb2,bg:#282828,hl:#fabd2f,fg+:#ebdbb2,bg+:#3c3836,hl+:#fabd2f,info:#83a598,prompt:#bdae93,spinner:#fabd2f,pointer:#83a598,marker:#fe8019,header:#665c54'

export FZF_DEFAULT_OPTS="
       --height ~60%
       --layout reverse
       --info inline-right
       --border rounded
       --multi
       --cycle
       --highlight-line
       --prompt '∼ '
       --marker '✓'
       --pointer '▶'
       --separator '─'
       --color '${_fzf_palette}'
       --bind 'ctrl-/:toggle-preview'
       --bind 'alt-u:preview-page-up'
       --bind 'alt-d:preview-page-down'
       --bind 'ctrl-a:toggle-all'
       --bind 'ctrl-t:toggle-all'
       "

# CTRL-T: paste selected files/dirs onto command-line
# NOTE: --walker-skip below is only a fallback when fd is absent; exclusions must also be in the fd command.
(($+commands[fd])) && export FZF_CTRL_T_COMMAND="fd -H -L -E .git -E node_modules -E target -E .venv -E __pycache__ --strip-cwd-prefix"
export FZF_CTRL_T_OPTS="
       --walker-skip .git,node_modules,target,.venv,__pycache__
       --scheme path
       --preview '(bat -n --color=always {} || eza -1 --color=always {}) 2>/dev/null'
       --preview-window 'hidden'
       --bind 'ctrl-/:change-preview-window(right,50%|hidden|)'
       --header 'CTRL-/ to toggle preview'
       "

# Platform-aware clipboard
typeset -g _clip_cmd
if (($+commands[pbcopy])); then
    _clip_cmd="pbcopy"
elif (($+commands[xclip])); then
    _clip_cmd="xclip -selection clipboard"
elif (($+commands[xsel])); then
    _clip_cmd="xsel --clipboard --input"
else
    _clip_cmd="cat > /dev/null"
fi

# CTRL-R: paste selected history command onto command-line
export FZF_CTRL_R_OPTS="
       --scheme history
       --wrap
       --bind 'ctrl-y:execute-silent(echo -n {2..} | ${_clip_cmd})+abort'
       --color 'header:#665c54:italic'
       --header 'CTRL-Y to copy to clipboard'
       "

# ALT-C: cd into selected directory
# NOTE: --walker-skip below is only a fallback when fd is absent; exclusions must also be in the fd command.
(($+commands[fd])) && export FZF_ALT_C_COMMAND="fd -t d -H -L -E .git -E node_modules -E target -E .venv -E __pycache__ --strip-cwd-prefix"
export FZF_ALT_C_OPTS="
       --walker-skip .git,node_modules,target,.venv,__pycache__
       --scheme path
       --preview '(eza --tree --icons --level 3 --color=always --group-directories-first {} || tree -NC {} || ls -1 {}) 2>/dev/null | head -100'
       --preview-window 'right,40%'
       "

# Customize fzf ** completion to use fd (exclusions match FZF_CTRL_T_COMMAND)
if (($+commands[fd])); then
    _fzf_compgen_path() {
        fd --hidden --follow -E .git -E node_modules -E target -E .venv -E __pycache__ . "$1"
    }
    _fzf_compgen_dir() {
        fd --type d --hidden --follow -E .git -E node_modules -E target -E .venv -E __pycache__ . "$1"
    }
fi

# Color man pages
if (($+commands[bat])); then
    export MANPAGER="sh -c 'col -bx | bat -l man -p'"
    export MANROFFOPT='-c'
else
    export LESS_TERMCAP_mb=$'\E[01;32m'
    export LESS_TERMCAP_md=$'\E[01;32m'
    export LESS_TERMCAP_me=$'\E[0m'
    export LESS_TERMCAP_se=$'\E[0m'
    export LESS_TERMCAP_so=$'\E[01;47;34m'
    export LESS_TERMCAP_ue=$'\E[0m'
    export LESS_TERMCAP_us=$'\E[01;36m'
fi
export LESS=-R

# see https://thevaluable.dev/zsh-completion-guide-examples
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
# Completer chain: complete → expand → match
zstyle ':completion:*' completer _complete _expand _match
zstyle ':completion:*:match:*' original only
# Group matches and describe
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'
# Cache completions (e.g. for apt, brew, pip)
zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path "${XDG_CACHE_HOME:-$HOME/.cache}/zsh/zcompcache"
zstyle ':completion:*' rehash true
zstyle ':completion:*' file-sort modification
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' verbose yes
zstyle ':completion:*' special-dirs true
zstyle ':completion:*' accept-exact '*(N)'
# Directories first in file completion
zstyle ':completion:*' list-dirs-first true

# ===== fzf-tab 配置 =====

zstyle ':completion:*:git-checkout:*' sort false
# fzf-tab-specific flags (fzf-tab does NOT follow FZF_DEFAULT_OPTS by default)
zstyle ':fzf-tab:*' fzf-flags \
    "--color=${_fzf_palette}" \
    '--bind=tab:accept'
zstyle ':fzf-tab:*' fzf-bindings 'ctrl-a:toggle-all' 'ctrl-/:toggle-preview'
zstyle ':fzf-tab:*' fzf-min-height 0
zstyle ':fzf-tab:*' fzf-pad 4
zstyle ':fzf-tab:*' switch-group ',' '.'

# 补全描述格式
zstyle ':completion:*:descriptions' format '[%d]'
# 文件名着色
(($+LS_COLORS)) && zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
# 禁用默认菜单，让 fzf-tab 接管
zstyle ':completion:*' menu no

# cd 补全时预览目录结构
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'eza -1 --color=always "$realpath"'

# 通用文件补全预览（bat 带语法高亮，目录用 eza）
zstyle ':fzf-tab:complete:*:*' fzf-preview \
    'if [[ -n "$realpath" ]]; then
         if [[ -d "$realpath" ]]; then
             eza -1 --color=always "$realpath"
         elif [[ -f "$realpath" ]]; then
             bat --style=numbers --color=always --line-range=:200 "$realpath" 2>/dev/null
         else
             echo "$realpath"
         fi
     fi'

# git - 按 group 区分预览
zstyle ':fzf-tab:complete:git-(add|diff|restore):*' fzf-preview 'git diff "$word" | delta'
zstyle ':fzf-tab:complete:git-log:*' fzf-preview 'git log --color=always "$word"'
zstyle ':fzf-tab:complete:git-help:*' fzf-preview 'git help "$word" | bat -plman --color=always'
zstyle ':fzf-tab:complete:git-show:*' fzf-preview \
    'case "$group" in
        "commit tag") git show --color=always "$word" ;;
        *) git show --color=always "$word" | delta ;;
    esac'
zstyle ':fzf-tab:complete:git-checkout:*' fzf-preview \
    'case "$group" in
        "modified file") git diff "$word" | delta ;;
        "recent commit object name") git show --color=always "$word" | delta ;;
        *) git log --color=always "$word" ;;
    esac'

# man
zstyle ':fzf-tab:complete:(\\|*/|)man:*' fzf-preview 'man "$word"'

# Kill - 进程预览（兼容 macOS BSD ps）
zstyle ':completion:*:*:*:*:processes' command 'ps -u $USER -o pid,user,comm -w -w'
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-preview \
    '[[ "$group" == "[process ID]" ]] && ps -p "$word" -o command= 2>/dev/null || echo "$word"'
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-flags --preview-window=down:3:wrap
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;36=0=01'
zstyle ':completion:*:*:kill:*' force-list always
zstyle ':completion:*:*:kill:*' insert-ids single

# Useful functions
# Extract archives
extract() {
    if [[ -f "$1" ]]; then
        case "$1" in
        *.tar.bz2) tar xjf "$1" ;;
        *.tar.gz) tar xzf "$1" ;;
        *.bz2) bunzip2 "$1" ;;
        *.rar) unrar e "$1" ;;
        *.gz) gunzip "$1" ;;
        *.tar) tar xf "$1" ;;
        *.tbz2) tar xjf "$1" ;;
        *.tgz) tar xzf "$1" ;;
        *.zip) unzip "$1" ;;
        *.Z) uncompress "$1" ;;
        *.7z) 7z x "$1" ;;
        *.tar.xz) tar xJf "$1" ;;
        *.tar.zst) zstd -dc "$1" | tar xf - ;;
        *.xz) xz -d "$1" ;;
        *.zst) zstd -d "$1" ;;
        *) echo "'$1' cannot be extracted via extract()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

# Create directory and cd into it
mkcd() {
    mkdir -p "$1" && cd "$1"
}

# Find and kill process by name
fkill() {
    local pid sig=${1:-9}
    pid=$(ps -u $UID -o pid,user,%cpu,%mem,start,command | sed 1d |
        fzf -m --header='[kill process]' --preview='ps -p {1} -o command=' |
        awk '{print $1}')
    [[ -n "$pid" ]] && echo "$pid" | xargs kill -"$sig"
}
