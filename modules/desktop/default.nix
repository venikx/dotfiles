{ config, lib, pkgs, ... }:

{
  imports = [ ./gaming ./terminal ./display-manager.nix ];

  hardware = {
    graphics.enable = true;
    graphics.enable32Bit = true;
  };
}
