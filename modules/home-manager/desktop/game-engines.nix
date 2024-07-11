{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.game-engines;
in {
  options.modules.desktop.game-engines = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable { home.packages = with pkgs; [ godot_4 ]; };
}
