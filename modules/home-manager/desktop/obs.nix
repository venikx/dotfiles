{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.obs;
in {
  options.modules.desktop.obs = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };
  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      handbrake
    ];

    programs.obs-studio = {
      enable = true;
      plugins = [];
    };
  };
}
