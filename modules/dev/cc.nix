{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.dev.cc;
in {
  options.modules.dev.cc = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.venikx = {
      home.packages = with pkgs; [
        clang
        clang-tools
      ];
    };
  };
}
