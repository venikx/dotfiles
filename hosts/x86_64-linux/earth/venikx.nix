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

  modules = {
    desktop = {
      obs.enable = true;
      emulators.enable = true;
    };
  };

  colorScheme = nix-colors.colorSchemes.tokyo-city-terminal-dark;

  home.stateVersion = "22.05";
}
