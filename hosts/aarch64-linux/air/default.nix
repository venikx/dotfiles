{ config, pkgs, lib, ... }:

{
  imports = [ ./hardware-configuration.nix ./apple-silicon-support ];

  programs.zsh.enable = true;
  users.users.venikx = {
    name = "venikx";
    description = "Kevin De Baerdemaeker";
    home = "/home/venikx";
    shell = pkgs.zsh;
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    initialPassword = "v3nikx";
  };
  time.timeZone = "Europe/Brussels";

  networking = {
    hostName = "air";
    useDHCP = lib.mkDefault true;
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

  modules = {
    audio = { pipewire.enable = true; };
    desktop = {
      bspwm.enable = true;
      terminal = {
        default = "xst";
        st.enable = true;
      };
    };
    dev = {
      cc.enable = true;
      clojure.enable = true;
      clojure.binaries.enable = false;
      rust.enable = true;
      rust.binaries.enable = false;
      shell.enable = true;
    };
    services = {
      docker.enable = true;
      tailscale.enable = true;
      syncthing.enable = true;
    };
    theme.active = "doom";
  };

  system.stateVersion = "23.11";
}
