[core]
    editor = nvim
    pager = delta

[color]
    ui = true

[pull]
    rebase = false

[interactive]
    diffFilter = delta --color-only

[delta]
    line-numbers = true
    diff-so-fancy = true
    navigate = true    # use n and N to move between diff sections

[merge]
    conflictstyle = diff3

[pager]
    difftool = true

[diff]
    tool = difftastic
    colorMoved = default

[difftool]
	prompt = false

[difftool "difftastic"]
	cmd = difft "$LOCAL" "$REMOTE"
