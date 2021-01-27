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
    # xst-256color isn't supported over ssh, so revert to a known one
    modules.shell.zsh.rcInit = ''
      [ "$TERM" = xst-256color ] && export TERM=xterm-256color
    '';

    home-manager.users.venikx = {
      home.packages = with pkgs; [
        xst  # st + nice-to-have extensions
        (makeDesktopItem {
          name = "xst";
          desktopName = "Suckless Terminal";
          genericName = "Default terminal";
          icon = "utilities-terminal";
          exec = "${xst}/bin/xst";
          categories = "Development;System;Utility";
        })
      ];
    };
  };
}
