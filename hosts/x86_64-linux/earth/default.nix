{
  pkgs,
  config,
  lib,
  ...
}:

{
  imports = [ ./hardware-configuration.nix ./disko-config.nix ];
  system.stateVersion = "26.05";

  environment.systemPackages = [
    pkgs.sbctl
  ];

  boot = {
    loader.systemd-boot.enable = lib.mkForce false;
    lanzaboote = {
      enable = true;
      pkiBundle = "/var/lib/sbctl";
    };
  };

  hardware.nvidia.open = false;
  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.stable;

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
    audio = {
      pipewire.enable = true;
    };
    desktop = {
      display-manager.enable = true;
      gaming.enable = true;
    };
    services = {
      docker.enable = true;
    };
  };

  fileSystems =
    let
      nasIP = "172.19.20.10";
    in
    {
      "/mnt/nas/entertainment" = {
        device = "${nasIP}:/mnt/tank/entertainment";
        fsType = "nfs";
        options = [
          "x-systemd.automount"
          "noauto"
        ];
      };

      #"/mnt/nas/documents" = {
      #  device = "//${nasIP}/documents";
      #  fsType = "cifs";
      #  options = [
      #    "nofail"
      #    "credentials=/etc/nixos/smb-secrets"
      #    "vers=3.0"
      #    "x-systemd.automount"
      #    "x-systemd.requires=network-online.target"
      #    "x-systemd.after=network-online.target"
      #    "uid=1000"
      #    "gid=100"
      #    "file_mode=0777"
      #    "dir_mode=0777"
      #  ];
      #};

      "/mnt/nas/images" = {
        device = "//${nasIP}/images";
        fsType = "cifs";
        options = [
          "nofail"
          "credentials=/etc/nixos/smb-secrets"
          "vers=3.0"
          "x-systemd.automount"
          "x-systemd.requires=network-online.target"
          "x-systemd.after=network-online.target"
          "uid=1000"
          "gid=100"
          "file_mode=0777"
          "dir_mode=0777"
        ];
      };
    };

}
