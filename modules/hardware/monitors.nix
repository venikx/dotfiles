{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.hardware.monitors;
in {
  options.modules.hardware.monitors = with types; {
    home.enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.home.enable {
    home-manager.users.venikx.programs.autorandr = {
      enable = true;
      profiles = {
        "default" = {
          fingerprint = {
            HDMI-0 = "*";
            DP-4 = "*";
          };
          config = {
            DP-4 = {
              enable = true;
              primary = true;
              rate = "144.00";
              mode = "2560x1440";
              dpi = 100;
            };
            HDMI-0 = {
              enable = true;
              dpi = 100;
              primary = false;
              mode = "1920x1080";
              rate = "60.00";
              rotate = "right";
              position = "2560x0";
            };
          };
        };
      };
    };
  };
}



