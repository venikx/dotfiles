{ config, lib, pkgs, ... }:

{
  imports =
    [
      ./browsers
      ./communication
      ./gaming
      ./media
      ./terminal
      ./bspwm.nix
      ./dmenu.nix
    ];
}
