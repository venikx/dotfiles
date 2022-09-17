{ config, lib, pkgs, ... }:

{
  imports =
    [
      ./desktop
      ./dev
      ./editors
      ./hardware
      ./shell
      ./services
      ./themes

      ./options.nix
    ];
}
