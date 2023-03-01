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
        reaper # full-featured DAW
        # ardour # FOSS DAW, can't run executable
        # lmms # midi and for 8bit music (for now)
        # zrythm # Modern alternative to lmms
        # renoise # tracker, but unstable
      ];
    };
  };
}
