{ options, config, lib, pkgs, ... }:

with lib;
{
  imports =
    [
      ./backup.nix
      ./spotify.nix
      ./streaming.nix
      ./music-production.nix
      ./readers.nix
    ];
}
