{ config, lib, pkgs, ... }:

{
  imports = [
    ./amd.nix
    ./audio.nix
    ./bluetooth.nix
    ./nvidia.nix
    ./monitors.nix
    ./networking.nix
  ];
}
