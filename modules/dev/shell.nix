{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.dev.shell;
in {
  options.modules.dev.shell = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.venikx = {
      home.packages = with pkgs; [
        shellcheck
      ];
    };
  };
}
