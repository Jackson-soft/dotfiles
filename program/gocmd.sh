#!/bin/bash
# Install essential Go development tools.
set -euo pipefail

if ! command -v go &>/dev/null; then
    echo "Error: go is not installed." >&2
    exit 1
fi

tools=(
    golang.org/x/tools/gopls
    github.com/go-delve/delve/cmd/dlv
    github.com/fatih/gomodifytags
)

for p in "${tools[@]}"; do
    echo "Installing ${p##*/} …"
    go install "${p}"@latest
done

echo "Done. All Go tools installed."
