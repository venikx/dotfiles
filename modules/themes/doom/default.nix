{ options, config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.theme;
  unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
in {
  config = mkIf (cfg.active == "doom") (mkMerge [
    # Desktop-agnostic configuration
    {
      modules = {
        theme = {
          wallpaper = mkDefault ./config/wallpaper.png;
          gtk = {
            theme = "Dracula";
            iconTheme = "Paper";
            cursorTheme = "Paper";
          };
        };

        shell.zsh.rcFiles  = [ ./config/zsh/prompt.zsh ];
      };
    }

    # Desktop (X11) theming
    (mkIf config.services.xserver.enable {
      home-manager.users.venikx = {
        home.packages = with pkgs; [
          unstable.dracula-theme
        ];

        # Other dotfiles
        xdg.configFile = with config.modules; mkMerge [
          {
            # Sourced from sessionCommands in modules/themes/default.nix
            "xtheme/doom".source = ./config/Xresources;
          }
          (mkIf desktop.bspwm.enable {
            "bspwm/rc.d/polybar".source = ./config/polybar/run.sh;
            "bspwm/rc.d/theme".source = ./config/bspwmrc;
            "polybar" = { source = ./config/polybar; recursive = true; };
            "dunst/dunstrc".source = ./config/dunstrc;
          })
        ];

      };

      fonts = {
        fonts = with pkgs; [
          fira-code
          fira-code-symbols
          jetbrains-mono
          siji
          font-awesome-ttf
        ];
        fontconfig.defaultFonts = {
          sansSerif = ["Fira Sans"];
          monospace = ["Fira Code"];
        };
      };

      # Compositor
      services.picom = {
        fade = true;
        fadeDelta = 1;
        fadeSteps = [ 0.01 0.012 ];
        shadow = true;
        shadowOffsets = [ (-10) (-10) ];
        shadowOpacity = 0.22;
        # activeOpacity = "1.00";
        # inactiveOpacity = "0.92";
        settings = {
          shadow-radius = 12;
          # blur-background = true;
          # blur-background-frame = true;
          # blur-background-fixed = true;
          blur-kern = "7x7box";
          blur-strength = 320;
        };
      };
    })
  ]);
}
