{ options, config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.desktop.media.backup;
in {
  options.modules.desktop.media.backup = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };

    driveDir = mkOption {
      type = str;
      default = "$XDG_DATA_HOME/pcloud";
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.venikx = {
      xsession.windowManager.bspwm.rules = {
        "pcloud" = {
          state = "tiled";
        };
      };
      home.packages = with pkgs; [
        pcloud
        # Get pcloud to keep its garbage out of $HOME
        # (writeScriptBin "pcloud" ''
        #   #!${stdenv.shell}
        #   HOME="${cfg.driveDir}" exec ${pcloud}/bin/pcloud "$@"
        # '')
        # # So a dmenu entry exists
        # (makeDesktopItem {
        #   name = "pcloud";
        #   desktopName = "Pcloud";
        #   icon = "pcloud";
        #   exec = "pcloud";
        #   terminal = "false";
        #   mimeType = "x-scheme-handler/pcloud";
        #   categories = "Network;FileTransfer";
        # })
      ];
    };
  };
}
