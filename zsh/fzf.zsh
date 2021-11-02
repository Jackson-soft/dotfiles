# fzf-tab
# disable sort when completing `git checkout`
zstyle ':completion:*:git-checkout:*' sort false
# preview directory's content with exa when completing cd
zstyle ':fzf-tab:complete:(z|cd):*' fzf-preview 'exa -1 --color=always $realpath'
# switch group using `,` and `.`
zstyle ':fzf-tab:*' switch-group ',' '.'

# fzf
# https://github.com/junegunn/fzf
export FZF_DEFAULT_COMMAND="fd -t f -H -L -E '.git' || git ls-tree -r --name-only HEAD || rg -f -. -L -g '!.git' || find ."
export FZF_CTRL_T_COMMAND=${FZF_DEFAULT_COMMAND}
export FZF_ALT_C_COMMAND="fd -t d"
export FZF_DEFAULT_OPTS="
       --layout=reverse
       --info=inline
       --height=60%
       --multi
       --border
       --preview-window=:hidden
       --preview '([[ -f {} ]] && (bat --style=numbers --color=always {} || cat {})) || ([[ -d {} ]] && (exa -T {} | less)) || echo {} 2> /dev/null | head -200'
       --color='hl:148,hl+:154,pointer:032,marker:010,bg+:237,gutter:008'
       --prompt='∼ ' --pointer='▶' --marker='✓'
       --bind '?:toggle-preview'
       --bind 'ctrl-a:select-all'
       --bind 'ctrl-y:execute-silent(echo {+} | pbcopy)'
       --bind 'ctrl-e:execute(echo {+} | xargs -o nvim)'
       --bind 'ctrl-v:execute(code {+})'
       "

# Use fd (https://github.com/sharkdp/fd) instead of the default find
# command for listing path candidates.
# - The first argument to the function ($1) is the base path to start traversal
# - See the source code (completion.{bash,zsh}) for the details.
_fzf_compgen_path() {
    fd --hidden --follow --exclude ".git" . "$1"
}

# Use fd to generate the list for directory completion
_fzf_compgen_dir() {
    fd --type d --hidden --follow --exclude ".git" . "$1"
}

# (EXPERIMENTAL) Advanced customization of fzf options via _fzf_comprun function
# - The first argument to the function is the name of the command.
# - You should make sure to pass the rest of the arguments to fzf.
_fzf_comprun() {
    local command=$1
    shift

    case "$command" in
        cd|z)         fzf "$@" --preview 'exa -T {} | head -200' ;;
        export|unset) fzf "$@" --preview "eval 'echo \$'{}" ;;
        ssh)          fzf "$@" --preview 'dig {}' ;;
        *)            fzf "$@" ;;
    esac
}

# GIT heart FZF
# @see https://junegunn.kr/2016/07/fzf-git/
# CTRL-G CTRL-F for files
# CTRL-G CTRL-B for branches
# CTRL-G CTRL-T for tags
# CTRL-G CTRL-R for remotes
# CTRL-G CTRL-H for commit hashes
is_in_git_repo() {
    git rev-parse HEAD > /dev/null 2>&1
}

fzf-down() {
    fzf --height 50% --min-height 20 --border --bind ctrl-/:toggle-preview "$@"
}

_gf() {
    is_in_git_repo || return
    git -c color.status=always status --short |
        fzf-down -m --ansi --nth 2..,.. \
                 --preview '(git diff --color=always -- {-1} | sed 1,4d; cat {-1})' |
        cut -c4- | sed 's/.* -> //'
}

_gb() {
    is_in_git_repo || return
    git branch -a --color=always | grep -v '/HEAD\s' | sort |
        fzf-down --ansi --multi --tac --preview-window right:70% \
                 --preview 'git log --oneline --graph --date=short --color=always --pretty="format:%C(auto)%cd %h%d %s" $(sed s/^..// <<< {} | cut -d" " -f1)' |
        sed 's/^..//' | cut -d' ' -f1 |
        sed 's#^remotes/##'
}

_gt() {
    is_in_git_repo || return
    git tag --sort -version:refname |
        fzf-down --multi --preview-window right:70% \
                 --preview 'git show --color=always {}'
}

_gh() {
    is_in_git_repo || return
    git log --date=short --format="%C(green)%C(bold)%cd %C(auto)%h%d %s (%an)" --graph --color=always |
        fzf-down --ansi --no-sort --reverse --multi --bind 'ctrl-s:toggle-sort' \
                 --header 'Press CTRL-S to toggle sort' \
                 --preview 'grep -o "[a-f0-9]\{7,\}" <<< {} | xargs git show --color=always' |
        grep -o "[a-f0-9]\{7,\}"
}

_gr() {
    is_in_git_repo || return
    git remote -v | awk '{print $1 "\t" $2}' | uniq |
        fzf-down --tac \
                 --preview 'git log --oneline --graph --date=short --pretty="format:%C(auto)%cd %h%d %s" {1}' |
        cut -d$'\t' -f1
}

_gs() {
    is_in_git_repo || return
    git stash list | fzf-down --reverse -d: --preview 'git show --color=always {1}' |
        cut -d: -f1
}

join-lines() {
    local item
    while read item; do
        echo -n "${(q)item} "
    done
}

bind-git-helper() {
    local c
    for c in $@; do
        eval "fzf-g$c-widget() { local result=\$(_g$c | join-lines); zle reset-prompt; LBUFFER+=\$result }"
        eval "zle -N fzf-g$c-widget"
        eval "bindkey '^g^$c' fzf-g$c-widget"
    done
}
bind-git-helper f b t r h s
unset -f bind-git-helper
