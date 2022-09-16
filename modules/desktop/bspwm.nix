{ options, config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.desktop.bspwm;
  configDir = config.dotfiles.configDir;
in {
  options.modules.desktop.bspwm = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    #modules.theme.onReload.bspwm = ''
    #  ${pkgs.bspwm}/bin/bspc wm -r
    #  source $XDG_CONFIG_HOME/bspwm/bspwmrc
    #'';

    environment.systemPackages = with pkgs; [
      xclip
      lightdm
      dunst
      libnotify
      (polybar.override {
        pulseSupport = true;
        nlSupport = true;
      })
    ];

    services = {
      picom.enable = true;
      redshift.enable = true;
      xserver = {
        enable = true;
        libinput.enable = true;
        displayManager = {
          defaultSession = "none+bspwm";
          lightdm.enable = true;
          lightdm.greeters.gtk.enable = true;
        };

        windowManager.bspwm.enable = true;
      };
    };

    systemd.user.services."dunst" = {
      enable = true;
      description = "";
      wantedBy = [ "default.target" ];
      serviceConfig.Restart = "always";
      serviceConfig.RestartSec = 2;
      serviceConfig.ExecStart = "${pkgs.dunst}/bin/dunst";
    };

    home-manager.users.venikx = {
      xsession.windowManager.bspwm = {
        enable = true;
        monitors = {
          DP-4 = ["1" "2" "3" "4" "5" "6" "7" "8" "9" "10"];
        };
        settings = {
          "remove_disabled_monitors" = true;
          "remove_unplugged_monitors" = true;
          "focus_follows_pointer" = true;
          "split_ratio" = 0.52;
        };
        startupPrograms = [ "sxhkd" "autorandr -c" "pcloud" ];
      };
      services.sxhkd = {
        enable = true;
        keybindings = {
          # Shutting down the system
          "super + shift + x" = ''prompt "Shuwdown computer?" "shutdown -h now"'';
          "super + shift + BackSpace" = ''prompt "Reboot computer?" "reboot"'';
          "super + shift + Escape" = ''prompt "Leave Xorg?" "killall Xorg"'';
          # Restarts bspwm (most when testing out configurations)
          "super + shift + r" = "bspc wm -r";
          "super + {_,shift + }q" = "bspc node -{c,k}";

          # Opening common commands
          "super + Return" = "$TERMINAL";
          "super + d" = "dmenu_run";
          "super + w" = "$BROWSER";

          # Moving around windows
          "super + {_,shift +}{1-9,0}" = "bspc {desktop -f, node -d} {1-9,10}";
          "super + {h,j,k,l}" = "bspc node -f {west,south,north,east}";
          "super + shift + {h,j,k,l}" = "bspc node -s {west,south,north,east}";
          "super + {_,ctrl + }f" = "bspc node -t ~{fullscreen,floating}";
          "super + space" = "bspc node -s biggest.local || bspc node -s next.local"; # swap windows

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
    };
  };
}
