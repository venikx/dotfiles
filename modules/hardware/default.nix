{ config, lib, pkgs, ... }:

{
  imports = [
    ./amd.nix
    ./audio.nix
    ./bluetooth.nix
    ./nvidia.nix
    ./networking.nix
  ];
}
