{ ... }:

{
  imports = [
    ./hardware-configuration.nix
  ];

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
    hostName = "dreamscape";
    useDHCP = false;
    interfaces.eno1.useDHCP = true; # ethernet
  };

  modules = {
    desktop = {
      bspwm.enable = true;
      dmenu.enable = true;
      media = {
        spotify.enable = true;
        streaming.enable = true;
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

