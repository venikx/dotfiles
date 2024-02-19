{ options, config, lib, pkgs, ... }:

with lib; {
  imports = [ ./steam.nix ./epic.nix ];
}
