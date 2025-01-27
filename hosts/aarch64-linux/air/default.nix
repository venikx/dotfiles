{ config, pkgs, lib, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  users.users.venikx = {
    name = "venikx";
    description = "Kevin De Baerdemaeker";
    home = "/home/venikx";
    shell = pkgs.zsh;
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    initialPassword = "v3nikx";
  };

  networking = {
    hostName = "air";
    useDHCP = lib.mkDefault true;
    firewall.checkReversePath = false; # wireguard
  };
  systemd.services.NetworkManager-wait-online.enable = lib.mkForce false;

  services.syncthing.settings = {
    devices = {
      "earth-nixos" = {
        id = "QUIA5TB-Q62NJOZ-NXNZPLY-6YXHXEJ-W5A6YMS-LTY2PXH-AZE5YHQ-22SHBQL";
      };
    };
    folders = { "org" = { devices = [ "earth-nixos" ]; }; };
  };

  services.libinput = {
    enable = true;
    touchpad.disableWhileTyping = true;
  };

  modules = {
    audio = { pipewire.enable = true; };
    desktop = { display-manager.enable = true; };
    services = {
      docker.enable = true;
      syncthing.enable = true;
      bluetooth.enable = true;
    };
  };

  system.stateVersion = "23.11";
}
