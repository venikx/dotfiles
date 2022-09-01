{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.media.video;
in {
  options.modules.desktop.media.video = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.venikx = {
      home.packages = with pkgs; [
        mpv
        vlc # in case mpv doesn't work
      ];
    };
  };
}
