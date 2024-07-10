{ config, lib, pkgs, ... }:

{
  imports = [ ./gaming ./terminal ./bspwm.nix ];

  hardware = {
    graphics.enable = true;
    graphics.enable32Bit = true;
  };
}
