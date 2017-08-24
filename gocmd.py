#!/usr/bin/python3
# -*- coding: utf-8 -*-

import os

cmds = [ 
    "go get -u -v github.com/nsf/gocode",
    "go get -u -v golang.org/x/tools/cmd/oracle",
    "go get -u -v golang.org/x/tools/cmd/goimports", 
    "go get -u github.com/alecthomas/gometalinter",
    "go get -u github.com/derekparker/delve/cmd/dlv",  
    "go get -u -v github.com/rogpeppe/godef",
    "go get -u github.com/golang/lint/golint",
    "go get -u -v github.com/zmb3/gogetdoc",
    "go get -u -v github.com/lukehoban/go-outline",
    "go get -u -v sourcegraph.com/sqs/goreturns",
    "go get -u -v golang.org/x/tools/cmd/gorename",
    "go get -u -v github.com/tpng/gopkgs",
    "go get -u -v github.com/newhook/go-symbols",
    "go get -u -v golang.org/x/tools/cmd/guru",
    "go get -u -v github.com/Masterminds/glide",
    "go get -u -v github.com/josharian/impl",
    "go get -u -v github.com/godoctor/godoctor",
    "go get -u -v github.com/fatih/gomodifytags",
    "go get -u -v github.com/cweill/gotests/...", ]

for i in cmds:
    print("start:==========", i)
    os.system(i)
