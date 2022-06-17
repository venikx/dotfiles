{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.browsers.brave;
in {
  options.modules.desktop.browsers.brave = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.venikx = {
      home.packages = with pkgs; [
        brave
        (makeDesktopItem {
          name = "brave-private";
          desktopName = "Brave Web Browser";
          genericName = "Open a private Brave window";
          icon = "brave";
          exec = "${brave}/bin/brave --incognito";
          categories = ["Network"];
        })
      ];
    };
  };
}
