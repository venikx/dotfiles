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
        audacity
      ];
      # TODO(Kevin): Currently not possible for lmms to define a different
      # configuration folder, so .lmmsrc.xml will be recreated in the home
      # folder. However the lmms folder itself (containing plugins etc) is
      # created in XDG_DOCUMENTS_DIR
    };
  };
}
