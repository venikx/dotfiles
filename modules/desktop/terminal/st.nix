{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.terminal.st;
in {
  options.modules.desktop.terminal.st = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.venikx = {
      home.packages = with pkgs; [
        xst  # st + nice-to-have extensions
        (makeDesktopItem {
          name = "xst";
          desktopName = "Suckless Terminal";
          genericName = "Default terminal";
          icon = "utilities-terminal";
          exec = "${xst}/bin/xst";
          categories = ["Development" "System" "Utility"];
        })
      ];
    };
  };
}
