{ options, config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.desktop.communication;
  unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
in {
  options.modules.desktop.communication = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.venikx = {
      home.packages = with pkgs; [
        unstable.discord
        slack
      ];
    };
  };
}
