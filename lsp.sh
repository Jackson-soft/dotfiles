#!/bin/bash

npms=(
	# formator
	prettier
	# linter
	js-yaml
	markdownlint-cli
	# lsp
	typescript
	yaml-language-server
	bash-language-server
	vscode-langservers-extracted # css json html eslint
	dockerfile-language-server-nodejs
	pyright # python
)

for p in "${npms[@]}"; do
	npm i -g "${p}"
done

pip3 install black mypy
