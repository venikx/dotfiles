#!/bin/sh

nix-channel --add https://github.com/nix-community/home-manager/archive/release-20.09.tar.gz home-manager
# nix-channel --add https://nixos.org/channels/nixos-unstable nixos
nix-channel --add https://nixos.org/channels/nixos-unstable nixos-unstable
nix-channel --update

