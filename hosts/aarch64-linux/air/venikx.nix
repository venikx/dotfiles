{ config, lib, pkgs, nix-colors, ... }:

{
  programs.autorandr = {
    enable = true;
    profiles = {
      "default" = {
        fingerprint = { eDP-1 = "*"; };
        config = {
          eDP-1 = {
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

  colorScheme = nix-colors.colorSchemes.tokyo-city-terminal-dark;

  home.stateVersion = "23.05";
}
