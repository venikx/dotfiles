{ options, config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.desktop.communication;
  unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
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
        # If not installed from the bleeding edge, Discord will sometimes
        # soft-lock itself on a "there's an update for discord" screen.
        (mkIf cfg.discord.enable unstable.discord)
        (mkIf cfg.slack.enable slack)
        (mkIf cfg.teams.enable teams)
      ];
    };
  };
}
