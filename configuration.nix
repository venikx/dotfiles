{ config, pkgs, lib, ... }:

{
  imports =
    [
      <home-manager/nixos>
      ./hosts/inception
      # ./hosts/dreamscape
      ./modules
    ];

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.permittedInsecurePackages = [
    "openssl-1.0.2u"
  ];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    coreutils
    pciutils
    git 
    vim
    unzip
    zip
    wget

    #TODO(Kevin) Move to other config folder
    xclip
    ranger
    screenkey
  ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?
}

