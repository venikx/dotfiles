{ config, lib, pkgs, ... }:

{
  imports =
    [
      ./browsers
      ./terminal
      ./bspwm.nix
      ./dmenu.nix
    ];
}
