{ config, lib, pkgs, ... }:

{
  imports =
    [
      ./gnupg.nix
      ./zsh.nix
    ];
}
