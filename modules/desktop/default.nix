{ config, lib, pkgs, ... }:

{
  imports = [ ./gaming ./display-manager.nix ];

  hardware = {
    graphics.enable = true;
    graphics.enable32Bit = true;
  };
}
