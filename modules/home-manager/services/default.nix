{ config, lib, pkgs, ... }:

{
  imports = [ ./docker.nix ./syncthing.nix ./yubikey.nix ];
}
