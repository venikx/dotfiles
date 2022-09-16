{ config, lib, pkgs, options, ... }:

with lib;
let
  cfg = config.modules.shell.direnv;
in {
  options.modules.shell.direnv = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.venikx = {
      programs.direnv = {
        enable = true;
        nix-direnv.enable = true;
        enableZshIntegration = true;
      };

      programs.git.ignores = [ ".direnv" ];
    };
  };
}
