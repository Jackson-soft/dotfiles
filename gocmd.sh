#!/bin/bash

go install golang.org/x/tools/cmd/goimports@latest
go install golang.org/x/tools/cmd/gorename@latest
go install golang.org/x/tools/cmd/guru@latest
go install golang.org/x/tools/gopls@latest
go install github.com/golangci/golangci-lint/cmd/golangci-lint@latest
go install github.com/go-delve/delve/cmd/dlv@latest
go install github.com/ramya-rao-a/go-outline@latest
go install github.com/uudashr/gopkgs/v2/cmd/gopkgs@latest
go install github.com/josharian/impl@latest
go install github.com/fatih/gomodifytags@latest
go install github.com/acroca/go-symbols@latest
go install github.com/davidrjenni/reftools/cmd/fillstruct@latest
go install github.com/jessfraz/dockfmt@latest
