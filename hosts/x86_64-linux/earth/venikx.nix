{ config, lib, pkgs, ... }:

{
  programs.autorandr = {
    enable = true;
    profiles = {
      "default" = {
        fingerprint = {
          DP-4 = "*";
        };
        config = {
          DP-4 = {
            enable = true;
            primary = true;
            rate = "144.00";
            mode = "2560x1440";
            dpi = 100;
          };
        };
      };
    };
  };

  modules = {
    desktop = {
      obs.enable = true;
    };
    dev = {
      nodejs.xdg.enable = true;
    };
  };

  home.stateVersion = "22.05";
}
