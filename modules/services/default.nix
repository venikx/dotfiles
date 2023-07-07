{ config, lib, pkgs, ... }:

{
  imports =
    [
      ./docker.nix
      ./tailscale.nix
    ];
}
