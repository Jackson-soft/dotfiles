#!/bin/zsh

## Options section
setopt EXTENDED_GLOB
setopt HIST_FCNTL_LOCK      # Use modern file-locking mechanisms, for better safety & performance.
setopt HIST_IGNORE_ALL_DUPS # Keep only the most recent copy of each duplicate entry in history.
setopt HIST_IGNORE_SPACE    # Don't record commands starting with space
setopt HIST_VERIFY          # Show command with history expansion to user before running it
setopt COMPLETE_IN_WORD     # Complete from both ends of a word.
setopt ALWAYS_TO_END        # Move cursor to the end of a completed word.
setopt PATH_DIRS            # Perform path search even on command names with slashes.
setopt AUTO_MENU            # Show completion menu on a successive tab press.
setopt AUTO_LIST            # Automatically list choices on ambiguous completion.
setopt AUTO_PARAM_SLASH     # If completed parameter is a directory, add a trailing slash.
setopt CORRECT              # Try to correct the spelling of commands.
setopt GLOB_DOTS            # Include dotfiles in globbing.
setopt MARK_DIRS            # Mark directories with trailing slash in filename completion.

# History settings
HISTSIZE=50000              # Increased history size
SAVEHIST=50000
HISTFILE="${ZDOTDIR:-$HOME}/.zsh_history"
setopt appendhistory
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY

# Set editor default keymap to emacs (`-e`) or vi (`-v`)
bindkey -e

alias -g ...='../..'
alias -g ....='../../..'

# fzf
# https://github.com/junegunn/fzf
export FZF_DEFAULT_COMMAND="fd -t f -H -L -E '.git' --strip-cwd-prefix || rg --files --hidden --follow --glob '!.git' || find . -type f"
export FZF_CTRL_T_COMMAND=$FZF_DEFAULT_COMMAND
export FZF_CTRL_T_OPTS="--preview '(bat --style=numbers --color=always {} || cat {} || eza -T {}) 2> /dev/null | head -200'"
export FZF_ALT_C_OPTS="--preview '(eza --tree --icons --level 3 --color=always --group-directories-first {} || tree -NC {} || ls --color=always --group-directories-first {}) 2>/dev/null | head -200'"
export FZF_DEFAULT_OPTS="
       --layout reverse
       --info inline-right
       --border rounded
       --multi
       --exact
       --cycle
       --highlight-line
       --prompt '∼ '
       --marker '✓'
       --pointer '▶'
       --separator '─'
       --color 'fg:#ebdbb2,bg:#282828,hl:#fabd2f'
       --color 'fg+:#ebdbb2,bg+:#3c3836,hl+:#fabd2f'
       --color 'info:#83a598,prompt:#bdae93,spinner:#fabd2f'
       --color 'pointer:#83a598,marker:#fe8019,header:#665c54'
       --preview-window :hidden
       --bind '?:toggle-preview'
       --bind 'ctrl-u:preview-page-up'
       --bind 'ctrl-d:preview-page-down'
       --bind 'ctrl-a:select-all'
       --bind 'ctrl-r:toggle-all'
       "

# Color man pages
export LESS_TERMCAP_mb=$'\E[01;32m'
export LESS_TERMCAP_md=$'\E[01;32m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;47;34m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;36m'
export LESS=-R

# see https://thevaluable.dev/zsh-completion-guide-examples
# zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' '+r:|?=**'
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
# Group matches and describe.
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'
zstyle ':completion::complete:*' use-cache on
zstyle ':completion:*' rehash true
zstyle ':completion:*' file-sort modification
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' verbose yes
zstyle ':completion:*' completer _complete _prefix _expand _correct _prefix _match _approximate
zstyle ':completion:*' special-dirs true
zstyle ':completion:*' accept-exact '*(N)'

# ===== fzf-tab 高级预览配置 =====

zstyle ':completion:*:git-checkout:*' sort false
zstyle ':fzf-tab:*' popup-min-size 50 8
zstyle ':fzf-tab:*' switch-group ',' '.'
zstyle ':fzf-tab:complete:cd:*' popup-pad 30 0

# 补全描述格式
zstyle ':completion:*:descriptions' format '[%d]'
# 文件名着色
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
# 禁用默认菜单，让 fzf-tab 接管
zstyle ':completion:*' menu no

# cd 补全时预览目录结构（需要 eza 或 exa）
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'eza -1 --color=always $realpath'

# 文件补全时预览文件内容（bat 带语法高亮）
zstyle ':fzf-tab:complete:*:*' fzf-preview '[[ -f $realpath ]] && bat --style=numbers --color=always --line-range=:200 $realpath || eza -1 --color=always $realpath'

# git checkout 补全分支时预览最近提交
zstyle ':fzf-tab:complete:git-checkout:*' fzf-preview 'git log --oneline --graph --decorate --color=always $(echo {}) -n 20'

# git checkout 补全 tag 时预览 tag 信息
zstyle ':fzf-tab:complete:git-checkout:refs/tags/*' fzf-preview 'git show --color=always $(echo {}) | bat --style=plain --color=always --line-range=:200'

# git checkout 补全 commit 时预览提交详情
zstyle ':fzf-tab:complete:git-checkout:commit' fzf-preview 'git show --color=always $(echo {}) | bat --style=plain --color=always --line-range=:200'

# 历史命令补全时预览完整命令
zstyle ':fzf-tab:complete:history-words:*' fzf-preview 'echo {} | fold -w $COLUMNS'

# git
zstyle ':fzf-tab:complete:git-(add|diff|restore):*' fzf-preview 'git diff $word | delta'
zstyle ':fzf-tab:complete:git-log:*' fzf-preview 'git log --color=always $word'
zstyle ':fzf-tab:complete:git-help:*' fzf-preview 'git help $word | bat -plman --color=always'
zstyle ':fzf-tab:complete:git-show:*' fzf-preview \
	'case "$group" in
	"commit tag") git show --color=always $word ;;
	*) git show --color=always $word | delta ;;
	esac'
zstyle ':fzf-tab:complete:git-checkout:*' fzf-preview \
	'case "$group" in
	"modified file") git diff $word | delta ;;
	"recent commit object name") git show --color=always $word | delta ;;
	*) git log --color=always $word ;;
	esac'

# man
zstyle ':fzf-tab:complete:(\\|*/|)man:*' fzf-preview 'man $word'

# Kill
# give a preview of commandline arguments when completing `kill`
zstyle ':completion:*:*:*:*:processes' command 'ps -u $USER -o pid,user,comm -w -w'
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-preview  \
    '[[ $group == "[process ID]" ]] && ps --pid=$word -o cmd --no-headers -w -w'
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-flags --preview-window=down:3:wrap
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;36=0=01'
zstyle ':completion:*:*:kill:*' force-list always
zstyle ':completion:*:*:kill:*' insert-ids single

# 手动加载一下环境变量
if [ -f ~/.zshenv ]; then
    source ~/.zshenv
fi

# Useful functions
# Extract archives
extract() {
    if [ -f $1 ]; then
        case $1 in
            *.tar.bz2)   tar xjf $1     ;;
            *.tar.gz)    tar xzf $1     ;;
            *.bz2)       bunzip2 $1     ;;
            *.rar)       unrar e $1     ;;
            *.gz)        gunzip $1      ;;
            *.tar)       tar xf $1      ;;
            *.tbz2)      tar xjf $1     ;;
            *.tgz)       tar xzf $1     ;;
            *.zip)       unzip $1       ;;
            *.Z)         uncompress $1  ;;
            *.7z)        7z x $1        ;;
            *)     echo "'$1' cannot be extracted via extract()" ;;
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
    local pid
    if [ "$UID" != "0" ]; then
        pid=$(ps -f -u $UID | sed 1d | fzf -m | awk '{print $2}')
    else
        pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')
    fi

    if [ "x$pid" != "x" ]; then
        echo $pid | xargs kill -${1:-9}
    fi
}

