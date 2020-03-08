#!/bin/zsh
set -e

echo
echo "------------------------------"
echo "Installing web development stuff..."

nvm install --lts

# Install yarn
npm i -g yarn

# Install dependencies for Emacs lsp-mode
npm i -g vscode-css-languageserver-bin
npm i -g vscode-html-languageserver-bin
npm i -g typescript-language-server typescript eslint
npm i -g vscode-json-languageserver
npm i -g vue-language-server
