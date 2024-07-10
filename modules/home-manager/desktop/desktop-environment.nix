{ config, pkgs, ... }:

{
  xsession.windowManager.bspwm = {
    enable = true;
    monitors = {
      DP-4 = [ "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" ];
      eDP-1 = [ "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" ]; # macbook
      eDP = [ "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" ]; # thrash laptop
    };
    settings = {
      "remove_disabled_monitors" = true;
      "remove_unplugged_monitors" = true;
      "focus_follows_pointer" = true;
      "split_ratio" = 0.52;
      "borderless_monocle" = true;
      "gapless_monocle" = true;
      "border_width" = 1;
      "window_gap" = 0;
      "normal_border_color" = "#181a23";
      "active_border_color" = "#181a23";
      "focused_border_color" = "#bd93f9";
      "presel_feedback_color" = "#bd93f9";
    };
    rules = {
      "steam_app_1284210" = { # Guild Wars 2 - Launcher
        state = "pseudo_tiled";
        #follow = true;
      };
    };
    startupPrograms = [ "sxhkd" "autorandr -c" ];
  };

  services.sxhkd = let
    ask = pkgs.writeShellScript "ask"
      ''[ "$(printf "No\\nYes" | dmenu -i -p "$1"  )" = "Yes" ] && $2'';
  in {
    enable = true;
    keybindings = {
      # Shutting down the system
      "super + shift + x" = ''${ask} "Shuwdown computer?" "shutdown -h now"'';
      "super + shift + BackSpace" = ''${ask} "Reboot computer?" "reboot"'';
      "super + shift + Escape" = ''${ask} "Leave Xorg?" "killall Xorg"'';
      # Restarts bspwm (most when testing out configurations)
      "super + shift + r" = "bspc wm -r";
      "super + {_,shift + }q" = "bspc node -{c,k}";

      # Opening common commands
      "super + Return" = "${pkgs.alacritty}/bin/alacritty";
      "super + d" = "dmenu_run";
      "super + w" = "$BROWSER";

      # Moving around windows
      "super + {_,shift +}{1-9,0}" = "bspc {desktop -f, node -d} {1-9,10}";
      "super + {h,j,k,l}" = "bspc node -f {west,south,north,east}";
      "super + shift + {h,j,k,l}" = "bspc node -s {west,south,north,east}";
      "super + {_,ctrl + }f" = "bspc node -t ~{fullscreen,floating}";
      "super + space" =
        "bspc node -s biggest.local || bspc node -s next.local"; # swap windows

      # TODO(Kevin): Media Keys
      "Print" = "scrcap";
      "XF86MonBrightnessUp" = "light -A 5";
      "XF86MonBrightnessDown" = "light -U 5";
      "XF86AudioMute" = "amixer -q set Master toggle";
      "XF86AudioLowerVolume" = "amixer -q set Master 10%- unmute";
      "XF86AudioRaiseVolume" = "amixer -q set Master 10%+ unmute";
      "XF86Audio{Play,Pause}" = "spt-send toggle";
      "XF86AudioNext" = "spt-send next";
      "XF86AudioPrev" = "spt-send prev";
    };
  };

  home.packages = with pkgs; [
    (dmenu.overrideAttrs
      (oldAttrs: rec { patches = [ ./dmenu/dmenu-xresources-4.9.diff ]; }))
    xclip
  ];

  home.file.backgrounds = { source = ./backgrounds; };

  services = {
    random-background = {
      enable = true;
      imageDirectory = "%h/backgrounds";
    };

    picom = {
      enable = true;
      fade = true;
      fadeDelta = 1;
      fadeSteps = [ 1.0e-2 1.2e-2 ];
      shadow = true;
      shadowOffsets = [ (-10) (-10) ];
      shadowOpacity = 0.22;
      settings = {
        shadow-radius = 12;
        blur-kern = "7x7box";
        blur-strength = 320;
      };
    };

    redshift = {
      enable = true;
      dawnTime = "07:00-09:00";
      duskTime = "19:00-21:00";
    };
    dunst.enable = true;
    dunst.configFile = ./dunst/dunstrc;
  };
}
