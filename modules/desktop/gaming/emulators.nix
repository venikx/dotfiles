{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.gaming.emulators;
in {
  options.modules.desktop.gaming.emulators = with types; {
    psx.enable = mkOption { type = bool; default = false; };
    ds.enable = mkOption { type = bool; default = false; };
    gba.enable = mkOption { type = bool; default = false; };
    gb.enable = mkOption { type = bool; default = false; };
    snes.enable = mkOption { type = bool; default = false; };
  };

  config = {
    home-manager.users.venikx = {
      home.packages = with pkgs; [
        (mkIf cfg.psx.enable epsxe)
        (mkIf cfg.ds.enable desmume)
        (mkIf (cfg.gba.enable || cfg.gb.enable  || cfg.snes.enable) higan)
      ];
    };
  };
}
