{ config, lib, pkgs, ... }:

{
  imports = [
    ./bluetooth.nix
    ./docker.nix
    ./networking.nix
    ./tailscale.nix
    ./syncthing.nix
  ];
}
