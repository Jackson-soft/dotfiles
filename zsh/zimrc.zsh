# Start configuration added by Zim install {{{
# -------
# Modules
# -------

# Sets sane Zsh built-in environment options.
zmodule environment
# Utility aliases and functions. Adds colour to ls, grep and less.
zmodule utility

#
# Prompt
#

# Additional completion definitions for Zsh.
zmodule zsh-users/zsh-completions
# Enables and configures smart and extensive tab completion.
# completion must be sourced after zsh-users/zsh-completions
zmodule completion
# Fish-like autosuggestions for Zsh.
zmodule zsh-users/zsh-autosuggestions
# Fish-like syntax highlighting for Zsh.
# zsh-users/zsh-syntax-highlighting must be sourced after completion
zmodule zsh-users/zsh-syntax-highlighting
# Fish-like history search (up arrow) for Zsh.
# zsh-users/zsh-history-substring-search must be sourced after zsh-users/zsh-syntax-highlighting
zmodule zsh-users/zsh-history-substring-search
# }}} End configuration added by Zim install

zmodule Aloxaf/fzf-tab
