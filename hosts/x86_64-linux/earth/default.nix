{ pkgs, config, ... }:

{
  imports = [ ./hardware-configuration.nix ];
  system.stateVersion = "22.05";

  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
  };

  hardware.nvidia.open = false;
  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.beta;

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

  fileSystems = let nasIP = "172.19.20.10";
  in {
    "/" = {
      device = "/dev/disk/by-uuid/4df3f4a4-fac6-471c-a8e1-11c03c2c093c";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/C831-07CF";
      fsType = "vfat";
    };

    "/mnt/nas/entertainment" = {
      device = "${nasIP}:/mnt/tank/entertainment";
      fsType = "nfs";
      options = [ "x-systemd.automount" "noauto" ];
    };

    "/mnt/nas/documents" = {
      device = "//${nasIP}/documents";
      fsType = "cifs";
      options = [
        "credentials=/etc/nixos/smb-secrets"
        "vers=3.0"
        "x-systemd.automount"
        "x-systemd.requires=network-online.target"
        "x-systemd.after=network-online.target"
        "uid=${toString config.users.users.syncthing.uid}"
        "gid=${toString config.users.groups.syncthing.gid}"
      ];
    };
  };

}
