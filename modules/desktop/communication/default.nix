{ options, config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.desktop.communication;
in {
  options.modules.desktop.communication = with types; {

    discord.enable = mkOption {
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

  config = {
    home-manager.users.venikx = {
      home.packages = with pkgs; [
        (mkIf cfg.discord.enable discord)
        (mkIf cfg.slack.enable slack)
        (mkIf cfg.teams.enable teams)
      ];
    };
  };
}
