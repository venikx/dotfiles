{ pkgs, home-manager, lib, ... }:

with lib;
{
  imports = [
    ./emacs.nix
  ];
}
