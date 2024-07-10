{ config, lib, pkgs, ... }:

{
  imports = [ ./gaming.nix ./display-manager.nix ];

  hardware = {
    graphics.enable = true;
    graphics.enable32Bit = true;
  };
}
