{ config, lib, pkgs, ... }:

{
  imports =
    [
      ./desktop
      ./dev
      ./editors
      ./hardware
      ./services
      ./themes

      ./options.nix
    ];
}
