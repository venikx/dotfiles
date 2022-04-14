{ config, pkgs, lib, ... }:

{
  imports =
    [
      <home-manager/nixos>
      ./hosts/dreamscape  # desktop
      # ./hosts/inception # laptop
      ./modules
    ];

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.permittedInsecurePackages = [
    "openssl-1.0.2u"
  ];

  environment.systemPackages = with pkgs; [
    coreutils
    pciutils
    git 
    vim
    unzip
    zip
    wget
    ranger
  ];
}
