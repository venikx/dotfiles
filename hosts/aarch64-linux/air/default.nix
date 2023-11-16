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

  services.syncthing.settings = {
    devices = {
      "kevin-iphone" = {
        id = "ELIRVGI-W7IUR4D-HRK25JP-HML6LPG-QEH76SY-BTAL4Q6-YO66RTD-SOP37AL";
      };
      "earth-nixos" = {
        id = "QUIA5TB-Q62NJOZ-NXNZPLY-6YXHXEJ-W5A6YMS-LTY2PXH-AZE5YHQ-22SHBQL";
      };
    };
    folders = {
      "org" = { # Name of folder in Syncthing, also the folder ID
        path = "/home/venikx/org/gtd"; # Which folder to add to Syncthing
        devices = [
          "kevin-iphone"
          "earth-nixos"
        ]; # Which devices to share the folder with
      };
    };
  };

  modules = {
    audio = { pipewire.enable = true; };
    desktop = {
      bspwm.enable = true;
      dmenu.enable = true;
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
