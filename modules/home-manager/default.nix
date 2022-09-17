{ config, lib, pkgs, ... }:

{
  home-manager.users.venikx.imports = [
    ./cli
  ];

  nixpkgs.config = {
    allowUnfree = true;
  };

}
