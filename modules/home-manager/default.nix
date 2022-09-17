{ config, lib, pkgs, ... }:

{
  home-manager.users.venikx.imports = [
    ./cli
    ./editors
  ];

  nixpkgs.config = {
    allowUnfree = true;
  };

}
