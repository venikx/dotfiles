{ config, lib, pkgs, ... }:

{
  imports =
    [
      ./browsers
      ./gaming
      ./media
      ./terminal
      ./bspwm.nix
      ./dmenu.nix
    ];
}
