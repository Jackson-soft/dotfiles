#!/usr/bin/python3
# -*- coding: utf-8 -*-

import os

cmds = [
    "go get -u -v github.com/nsf/gocode",
    "go get -u -v golang.org/x/tools/cmd/oracle",
    "go get -u -v golang.org/x/tools/cmd/goimports",
    "go get -u -v golang.org/x/tools/cmd/gorename",
    "go get -u -v golang.org/x/tools/cmd/guru",
    "go get -u github.com/alecthomas/gometalinter",
    "go get -u github.com/derekparker/delve/cmd/dlv",
    "go get -u -v github.com/rogpeppe/godef",
    "go get -u -v github.com/golang/lint/golint",
    "go get -u -v github.com/zmb3/gogetdoc",
    "go get -u -v github.com/ramya-rao-a/go-outline",
    "go get -u -v sourcegraph.com/sqs/goreturns",
    "go get -u -v github.com/uudashr/gopkgs/cmd/gopkgs",
    "go get -u -v github.com/josharian/impl",
    "go get -u -v github.com/godoctor/godoctor",
    "go get -u -v github.com/fatih/gomodifytags",
    "go get -u -v github.com/cweill/gotests/...",
    "go get -u github.com/golang/dep/cmd/dep",
    "go get -u github.com/sourcegraph/go-langserver",
    "go get -u -v github.com/acroca/go-symbols",
    "go get -u -v github.com/haya14busa/goplay/cmd/goplay",
]

for i in cmds:
    print("start:==========", i)
    os.system(i)
