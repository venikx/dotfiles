{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.dmenu;
in {
  options.modules.desktop.dmenu = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.venikx = {
      home.packages = with pkgs; [
        (dmenu.overrideAttrs (oldAttrs: rec {
          patches = [
            /etc/nixos/config/dmenu/dmenu-xresources-4.9.diff
          ];
        }))
      ];
    };
  };
}
