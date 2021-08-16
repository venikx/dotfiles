#!/bin/sh

nix-channel --add https://github.com/nix-community/home-manager/archive/release-21.05.tar.gz home-manager
nix-channel --add https://nixos.org/channels/nixos-21.05 nixos
nix-channel --add https://nixos.org/channels/nixos-unstable nixos-unstable
nix-channel --update
