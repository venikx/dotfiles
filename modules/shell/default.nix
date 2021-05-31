{ config, lib, pkgs, ... }:

{
  imports =
    [
      ./direnv.nix
      ./git.nix
      ./gnupg.nix
      ./zsh.nix
    ];
}
