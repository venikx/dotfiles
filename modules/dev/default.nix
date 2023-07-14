
{ config, lib, pkgs, ... }:

{
  imports =
    [
      ./cc.nix
      ./clojure.nix
      ./rust.nix
      ./shell.nix
    ];
}
