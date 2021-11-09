#!/bin/bash

if (($ + commands[go])); then
    go install golang.org/x/tools/gopls@latest
    go install github.com/golangci/golangci-lint/cmd/golangci-lint@latest
    go install github.com/go-delve/delve/cmd/dlv@latest
    go install github.com/fatih/gomodifytags@latest
fi
