#!/bin/bash

if command -v go >/dev/null 2>&1; then
	go install golang.org/x/tools/gopls@latest
	go install github.com/go-delve/delve/cmd/dlv@latest
	go install github.com/fatih/gomodifytags@latest
fi
