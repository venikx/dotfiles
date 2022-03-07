#!/bin/sh

sudo nix-channel --add https://github.com/nix-community/home-manager/archive/release-21.11.tar.gz home-manager
sudo nix-channel --add https://nixos.org/channels/nixos-21.11 nixos
sudo nix-channel --add https://nixos.org/channels/nixos-unstable nixos-unstable
sudo nix-channel --update
