{ pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

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
    hostName = "fire";
    useDHCP = false;
    interfaces.wlp2s0.useDHCP = true;
  };

  modules = {
    audio = { pipewire.enable = true; };
    desktop = {
      bspwm.enable = true;
      terminal = {
        default = "xst";
        st.enable = true;
      };

      gaming = { steam.enable = true; };
    };
    editors = { emacs.enable = true; };
    dev = {
      cc.enable = true;
      rust.enable = true;
      rust.binaries.enable = false;
      shell.enable = true;
    };
    services = {
      docker.enable = true;
      tailscale.enable = true;
      syncthing.enable = true;
      bluetooth.enable = true;
    };
    theme.active = "doom";
  };

  boot = {
    loader.efi.canTouchEfiVariables = true;
    loader.efi.efiSysMountPoint = "/boot";
    loader.grub.enable = true;
    loader.grub.devices = [ "nodev" ];
    loader.grub.efiSupport = true;
    loader.grub.useOSProber = true;
  };

  system.stateVersion = "23.05";
}
