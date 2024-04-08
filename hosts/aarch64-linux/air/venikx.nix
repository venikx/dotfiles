{ config, lib, pkgs, nix-colors, ... }:

{
  programs.autorandr = {
    enable = true;
    profiles = {
      "default" = {
        fingerprint = { None-1 = "*"; };
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

  modules = { dev = { nodejs.xdg.enable = true; }; };
  colorScheme = nix-colors.colorSchemes.tokyo-city-terminal-dark;

  home.stateVersion = "23.05";
}
