
{ config, lib, pkgs, ... }:

{
  imports =
    [
      ./cc.nix
      ./node.nix
      ./rust.nix
      ./shell.nix
    ];
}
