{
  self,
  osConfig,
  lib,
  pkgs,
  nix-colors,
  ...
}:

{
  programs.autorandr = {
    enable = true;
    profiles = {
      "default" = {
        fingerprint = {
          DP-0 = "*";
        };
        config = {
          DP-0 = {
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
    folders = {
      "org" = {
        devices = [ "air-nixos" ];
      };
    };
  };

  modules = {
    desktop = {
      emulators.enable = true;
    };
  };

  home.packages =
    [ ]
    ++ lib.optionals osConfig.services.pipewire.enable [
      pkgs.scarlett2
      pkgs.alsa-scarlett-gui
    ];

  colorScheme = nix-colors.colorSchemes.tokyo-city-terminal-dark;

  imports = [
    "${self}/modules/home-manager/desktop/video-production.nix"
    "${self}/modules/home-manager/desktop/photography.nix"
    "${self}/modules/home-manager/desktop/game-development.nix"
  ];

  home.stateVersion = "26.05";
}
