{ options, config, lib, pkgs, ... }:

with lib;
{
  imports =
    [
      ./spotify.nix
      ./streaming.nix
      ./music-production.nix
      ./readers.nix
    ];
}
