{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.gaming.steam;
in {
  options.modules.desktop.gaming.steam = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };

    libDir = mkOption {
      type = str;
      default = "$XDG_DATA_HOME/steamlib";
    };
  };

  config = mkIf cfg.enable {
    system.userActivationScripts.setupSteamDir = ''mkdir -p "${cfg.libDir}"'';

    programs.steam = {
      enable = true;
      remotePlay.openFirewall =
        true; # Open ports in the firewall for Steam Remote Play
      dedicatedServer.openFirewall =
        true; # Open ports in the firewall for Source Dedicated Server
    };

    home-manager.users.venikx = {
      home.packages = with pkgs;
        [
          ## Prevent steam from polluting $HOME
          #(writeScriptBin "steam" ''
          #  #!${stdenv.shell}
          #  HOME="${cfg.libDir}" exec ${steam}/bin/steam "$@"
          #'')

          ## To run DRM free games
          #(writeScriptBin "steam-run" ''
          #  #!${stdenv.shell}
          #  HOME="${cfg.libDir}" exec ${steam-run-native}/bin/steam-run "$@"
          #'')

          ## Needed for a dmenu entry
          #(makeDesktopItem {
          #  name = "steam";
          #  desktopName = "Steam";
          #  icon = "steam";
          #  exec = "steam";
          #  terminal = "false";
          #  mimeType = "x-scheme-handler/steam";
          #  categories = "Network;FileTransfer;Game";
          #})
        ];
    };
  };
}
