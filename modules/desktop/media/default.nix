{ options, config, lib, pkgs, ... }:

with lib;
{
  imports =
    [
      ./backup.nix
      ./music-production.nix
      ./readers.nix
      ./spotify.nix
      ./streaming.nix
      ./video.nix
    ];
}
