[core]
    editor = nvim
    pager = delta

[pull]
    rebase = false

[interactive]
    diffFilter = delta --color-only

[delta]
    features = decorations
    side-by-side = true
    line-numbers = true
    navigate = true
    relative-paths = true
    hyperlinks = true
    true-color = always
    syntax-theme = gruvbox-dark
    whitespace-error-style = 22 reverse

[delta "decorations"]
    commit-decoration-style = bold yellow box ul
    file-style = bold yellow ul
    file-decoration-style = none

[merge]
    conflictstyle = zdiff3

[diff]
    colorMoved = default
    colorMovedWS = allow-indentation-change
