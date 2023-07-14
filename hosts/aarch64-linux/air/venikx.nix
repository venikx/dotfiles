{ config, lib, pkgs, ... }:

{
  programs.autorandr = {
    enable = true;
    profiles = {
      "default" = {
        fingerprint = {
          None-1 = "*";
        };
        config = {
          None-1 = {
            enable = true;
            primary = true;
            rate = "60.00";
            mode = "2560x1600";
            dpi = 150;
          };
        };
      };
    };
  };

  modules = {
    desktop = {
      obs.enable = true;
    };
  };

  home.stateVersion = "23.05";
}
