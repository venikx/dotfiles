
{ config, lib, pkgs, ... }:

{
  imports =
    [
      ./cc.nix
      ./clojure.nix
      ./node.nix
      ./rust.nix
      ./shell.nix
    ];
}
