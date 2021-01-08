{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.dmenu;
in {
  options.modules.desktop.dmenu = with types; {
    enable = mkOption {
      type = bool;
      default = true;
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      dmenu
    ];
  };
}
