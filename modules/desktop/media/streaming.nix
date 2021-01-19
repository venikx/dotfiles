{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.media.streaming;
in {
  options.modules.desktop.media.streaming = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.venikx = {
      home.packages = with pkgs; [
        obs-studio
        audacity
        handbrake
      ];
    };
  };
}
