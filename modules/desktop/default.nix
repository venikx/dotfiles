{ config, lib, pkgs, ... }:

{
  imports =
    [
      ./gaming
      ./media
      ./terminal
      ./bspwm.nix
      ./dmenu.nix
    ];
}
