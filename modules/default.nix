{ config, lib, pkgs, ... }:

{
  imports =
    [
      ./desktop
      ./editors
      ./hardware
      ./shell
      ./services
      ./themes

      ./options.nix
      ./xdg.nix
    ];
}
