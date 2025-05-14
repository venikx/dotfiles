{ config, lib, pkgs, ... }:

{
  imports = [ ./bluetooth.nix ./docker.nix ./networking.nix ];
}
