{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.gaming.emulators;
in {
  options.modules.desktop.gaming.emulators = with types; {
    psx.enable = mkOption {
      type = bool;
      default = false;
    };
    ds.enable = mkOption {
      type = bool;
      default = false;
    };
    gba.enable = mkOption {
      type = bool;
      default = false;
    };
    gb.enable = mkOption {
      type = bool;
      default = false;
    };
    snes.enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = {
    home-manager.users.venikx = mkMerge [
      {
        home.packages = with pkgs; [
          (mkIf cfg.ds.enable desmume)
          (mkIf (cfg.gba.enable || cfg.gb.enable || cfg.snes.enable) higan)
        ];
      }
      (mkIf (cfg.gba.enable || cfg.gb.enable || cfg.snes.enable) {
        xdg.configFile."higan" = {
          source = "/etc/nixos/config/higan";
          recursive = true;
        };
      })
    ];
  };
}
