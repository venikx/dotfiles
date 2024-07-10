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
    services = {
      displayManager = { defaultSession = "none+bspwm"; };

      xserver = {
        enable = true;
        dpi = 100;
        windowManager.bspwm.enable = true;

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
