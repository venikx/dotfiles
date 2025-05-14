{ config, lib, pkgs, nix-colors, ... }:

{
  programs.autorandr = {
    enable = true;
    profiles = {
      "default" = {
        fingerprint = { DP-4 = "*"; };
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

  services.syncthing.settings = {
    devices = {
      "air-nixos" = {
        id = "WK7RS2C-362VDSU-6AADX3Q-AFTADBL-PNY3KJO-ALQ6HYO-S6MQMOU-6MSYYAR";
      };
    };
    folders = { "org" = { devices = [ "air-nixos" ]; }; };
  };

  modules = {
    desktop = {
      obs.enable = true;
      emulators.enable = true;
      game-engines.enable = true;
    };
  };

  colorScheme = nix-colors.colorSchemes.tokyo-city-terminal-dark;

  home.stateVersion = "22.05";
}
