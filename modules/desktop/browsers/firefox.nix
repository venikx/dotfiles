{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.browsers.firefox;
in {
  options.modules.desktop.browsers.firefox = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };
  # TODO(kevin): Configure settings
  # TODO(kevin): Setup Plugins (if that's still needed with Firefox Sync)

  config = mkIf cfg.enable {
    home-manager.users.venikx = {
      home.packages = with pkgs; [
        firefox
      ];
    };
  };
}
