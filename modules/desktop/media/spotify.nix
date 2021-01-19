{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.media.spotify;
in {
  options.modules.desktop.media.spotify = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.venikx = {
      home.packages = with pkgs; [
        spotify
      ];
    };
  };
}
