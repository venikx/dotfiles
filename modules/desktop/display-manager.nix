{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.display-manager;
in {
  options.modules.desktop.display-manager = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    services = {
      displayManager = { defaultSession = "none+bspwm"; };

      xserver = {
        enable = true;
        dpi = 100;
        windowManager.bspwm.enable = true;

        xautolock = {
          enable = true;
          nowlocker = "${pkgs.lightdm}/bin/dm-tool lock";
          locker = "${pkgs.lightdm}/bin/dm-tool lock";
          notifier =
            "${pkgs.libnotify}/bin/notify-send 'Move boy! Locking in 10 seconds...'";
        };

        displayManager = {
          lightdm = {
            enable = true;
            greeters.gtk.enable = true;
            background = ./login-screen.png;
          };
        };
      };
    };
  };
}
