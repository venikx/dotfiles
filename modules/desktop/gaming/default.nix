{ options, config, lib, pkgs, ... }:

with lib; {
  imports = [
    ./emulators.nix
    ./steam.nix
    #./epic.nix
  ];
}
