{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.theme;
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
      };

      home-manager.users.venikx.programs.zsh.initExtraBeforeCompInit = ''
        source ${./config/zsh/prompt.zsh}
      '';
    }

    # Desktop (X11) theming
    (mkIf config.services.xserver.enable {
      home-manager.users.venikx = {
        home.packages = with pkgs; [ dracula-theme ];

        xsession.windowManager.bspwm.settings = {
          "borderless_monocle" = true;
          "gapless_monocle" = true;
          "border_width" = 1;
          "window_gap" = 0;
          "normal_border_color" = "#181a23";
          "active_border_color" = "#181a23";
          "focused_border_color" = "#bd93f9";
          "presel_feedback_color" = "#bd93f9";
        };

        # Other dotfiles
        xdg.configFile = with config.modules;
          mkMerge [
            {
              # Sourced from sessionCommands in modules/themes/default.nix
              "xtheme/doom".source = ./config/Xresources;
            }
            (mkIf desktop.bspwm.enable {
              "dunst/dunstrc".source = ./config/dunstrc;
            })
          ];

      };

      fonts = {
        packages = with pkgs; [
          barlow
          fira-code
          fira-code-symbols
          jetbrains-mono
          siji
          font-awesome
        ];
        fontconfig.defaultFonts = {
          sansSerif = [ "Fira Sans" ];
          monospace = [ "Fira Code" ];
        };
      };

      # Compositor
      services.picom = {
        fade = true;
        fadeDelta = 1;
        fadeSteps = [ 1.0e-2 1.2e-2 ];
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
