#!/bin/bash

npms=(
	# formator
	prettier
	# linter
	markdownlint-cli
	# lsp
	typescript
	yaml-language-server
	bash-language-server
	vscode-langservers-extracted      # css json html eslint
	dockerfile-language-server-nodejs # docker
	pyright                           # python
	dot-language-server               # dot
)

for p in "${npms[@]}"; do
	npm i -g "${p}"
done
