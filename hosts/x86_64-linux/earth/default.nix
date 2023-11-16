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

  networking = {
    hostName = "earth";
    useDHCP = false;
    interfaces.eno1.useDHCP = true; # ethernet
  };

  services.syncthing.settings = {
    devices = {
      "kevin-iphone" = {
        id = "ELIRVGI-W7IUR4D-HRK25JP-HML6LPG-QEH76SY-BTAL4Q6-YO66RTD-SOP37AL";
      };
      "air-nixos" = {
        id = "WK7RS2C-362VDSU-6AADX3Q-AFTADBL-PNY3KJO-ALQ6HYO-S6MQMOU-6MSYYAR";
      };
    };
    folders = {
      "org" = { # Name of folder in Syncthing, also the folder ID
        path = "/home/venikx/org/gtd"; # Which folder to add to Syncthing
        devices = [
          "kevin-iphone"
          "air-nixos"
        ]; # Which devices to share the folder with
      };
    };
  };

  modules = {
    audio = { pipewire.enable = true; };
    desktop = {
      bspwm.enable = true;
      dmenu.enable = true;
      media = {
        spotify.enable = true;
        readers = {
          pdf.enable = true;
          ebook.enable = true;
        };
        music-production.enable = true;
        video.enable = true;
      };
      terminal = {
        default = "xst";
        st.enable = true;
      };
      gaming = {
        steam.enable = true;
        epic.enable = false;
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
      syncthing.enable = true;
    };
    theme.active = "doom";
  };

  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
  };

  system.stateVersion = "22.05";
}

