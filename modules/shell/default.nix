{ config, lib, pkgs, ... }:

{
  imports =
    [
      ./zsh.nix
      ./git.nix
    ];
}
