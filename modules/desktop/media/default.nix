{ options, config, lib, pkgs, ... }:

with lib;
{
  imports =
    [
      ./music-production.nix
      ./readers.nix
      ./spotify.nix
      ./streaming.nix
      ./video.nix
    ];
}
