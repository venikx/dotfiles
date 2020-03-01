#!/bin/bash
set -e

echo
echo "------------------------------"
echo "Installing rust-lang..."
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | zsh -s -- -y && source ~/.cargo/env

echo
echo "------------------------------"
echo "Installing C/C++ stuff..."
