{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.media.readers;
in {
  options.modules.desktop.media.readers = with types; {
    pdf.enable = mkOption {
      type = bool;
      default = false;
    };

    ebook.enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = {
    home-manager.users.venikx = {
      home.packages = with pkgs; [
        (mkIf cfg.pdf.enable evince)
        (mkIf cfg.ebook.enable calibre)
      ];
    };
  };
}
