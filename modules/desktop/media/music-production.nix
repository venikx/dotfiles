{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.media.music-production;
in {
  options.modules.desktop.media.music-production = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.venikx = {
      home.packages = with pkgs; [
        lmms
        # TODO(kevin): Explore sunvox
        audacity
      ];
    };
  };
}
