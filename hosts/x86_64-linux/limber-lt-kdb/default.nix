{ pkgs, ... }:

{
  imports = [ ./disko-config.nix ./hardware-configuration.nix ];
  system.stateVersion = "24.11";

  boot = {
    tmp.cleanOnBoot = true;
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
  };

  networking = {
    hostName = "limber-lt-kdb";
    useDHCP = false;
    interfaces.wlp2s0.useDHCP = true;
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
    desktop = { display-manager.enable = true; };
    services = {
      docker.enable = true;
      #syncthing.enable = true;
    };
  };
}
