{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.gaming.epic;
in {
  options.modules.desktop.gaming.epic = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.venikx = {
      home.packages = with pkgs; [ lutris legendary-gl heroic ];
    };
  };
}
