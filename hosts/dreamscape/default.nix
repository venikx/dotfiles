{ ... }:

{
  imports = [
    ../../modules
    ../../modules/common.nix
    ../../modules/nixos
    ../nix.nix
    ../linux.nix
    ./hardware-configuration.nix
  ];

  networking = {
    hostName = "dreamscape";
    networkmanager.enable = true;
    useDHCP = false;
    interfaces.eno1.useDHCP = true; # ethernet
    interfaces.wlp5s0.useDHCP = true; # wifi
  };

  modules = {
    desktop = {
      bspwm.enable = true;
      dmenu.enable = true;
      communication = {
        discord.enable = true;
        slack.enable = true;
      };
      browsers = {
        default = "brave";
        firefox.enable = true;
        brave.enable = true;
      };
      media = {
        spotify.enable = true;
        streaming.enable = true;
        readers = {
          pdf.enable = true;
          ebook.enable = true;
        };
        backup.enable = true;
        music-production.enable = true;
        video.enable = true;
      };
      terminal = {
        default = "xst";
        st.enable = true;
      };
      gaming = {
        steam.enable = true;
        epic.enable = true;
      };
    };
    editors = {
      emacs.enable = true;
    };
    dev = {
      cc.enable = true;
      clojure.enable = true;
      clojure.binaries.enable = false;
      node.enable = true;
      node.binaries.enable = false;
      rust.enable = true;
      rust.binaries.enable = false;
      shell.enable = true;
    };
    shell = {
      zsh.enable = true;
    };
    services = {
      docker.enable = true;
    };
    theme.active = "doom";
  };

  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
  };
}

