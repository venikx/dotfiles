{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.emulators;
in {
  options.modules.desktop.emulators = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ desmume higan ];

    xdg.configFile."higan" = {
      source = ./higan;
      recursive = true;
    };
  };
}
