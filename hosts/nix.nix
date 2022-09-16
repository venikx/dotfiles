{ config, pkgs, lib, ... }:

{
  nix = {
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    gc.automatic = true;
  };

  nixpkgs.config.allowUnfree = true;

  # can I remove this?
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
