{ pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];
  system.stateVersion = "22.05";

  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
  };

  networking = {
    hostName = "earth";
    useDHCP = false;
    interfaces.eno1.useDHCP = true; # ethernet
  };

  users.users.venikx = {
    name = "venikx";
    description = "Kevin De Baerdemaeker";
    home = "/home/venikx";
    shell = pkgs.zsh;
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    initialPassword = "v3nikx";
  };

  modules = {
    audio = { pipewire.enable = true; };
    desktop = {
      display-manager.enable = true;
      gaming.enable = true;
    };
    services = {
      docker.enable = true;
      syncthing.enable = true;
    };
  };

  services.syncthing.settings = {
    devices = {
      "air-nixos" = {
        id = "WK7RS2C-362VDSU-6AADX3Q-AFTADBL-PNY3KJO-ALQ6HYO-S6MQMOU-6MSYYAR";
      };
    };
    folders = { "org" = { devices = [ "air-nixos" ]; }; };
  };
}
