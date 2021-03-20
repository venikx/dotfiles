{ options, config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.desktop.media.backup;
  unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
in {
  options.modules.desktop.media.backup = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };

    slack.enable = mkOption {
      type = bool;
      default = false;
    };

    teams.enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.venikx = {
      home.packages = with pkgs; [
        unstable.pcloud
      ];
    };
  };
}
