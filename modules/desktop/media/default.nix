{ options, config, lib, pkgs, ... }:

with lib; {
  imports = [ ./music-production.nix ./readers.nix ./video.nix ];
}
