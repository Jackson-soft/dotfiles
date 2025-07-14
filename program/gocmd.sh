#!/bin/bash

tools=(
    golang.org/x/tools/gopls
    github.com/go-delve/delve/cmd/dlv
    github.com/fatih/gomodifytags
    github.com/golangci/golangci-lint/v2/cmd/golangci-lint
)

if command -v go >/dev/null 2>&1; then
    for p in "${tools[@]}"; do
        go install "${p}"@latest
    done
fi
