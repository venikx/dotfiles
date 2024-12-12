{ config, nix-colors, ... }:

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
            mode = "2880x1920";
            dpi = 150;
          };
        };
      };
    };
  };

  colorScheme = nix-colors.colorSchemes.tokyo-city-terminal-dark;
  home.stateVersion = "24.11";
}
