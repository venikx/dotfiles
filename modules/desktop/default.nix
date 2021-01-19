{ config, lib, pkgs, ... }:

{
  imports =
    [
      ./browsers
      ./communication
      ./media
      ./terminal
      ./bspwm.nix
      ./dmenu.nix
    ];
}
