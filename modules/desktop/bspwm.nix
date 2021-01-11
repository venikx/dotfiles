{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.bspwm;
in {
  options.modules.desktop.bspwm = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    modules.theme.onReload.bspwm = ''
      ${pkgs.bspwm}/bin/bspc wm -r
      source $XDG_CONFIG_HOME/bspwm/bspwmrc
    '';

    environment.systemPackages = with pkgs; [
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
        # TODO(kevin): Part of the device specific configuration as desktops don't have touchpads
        libinput.enable = true;
        displayManager = {
          defaultSession = "none+bspwm";
          lightdm.enable = true;
          lightdm.greeters.gtk.enable = true;
        };

        videoDrivers = [ "amdgpu" ];
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

    # link recursively so other modules can link files in their folders
    home-manager.users.venikx = {
      xdg.configFile = {
        "sxhkd".source = "/etc/nixos/config/sxhkd";
        "bspwm" = {
          source = "/etc/nixos/config/bspwm";
          recursive = true;
        };
      };
    };
  };
}
