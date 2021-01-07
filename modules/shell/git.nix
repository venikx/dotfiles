{ config, lib, pkgs, options, ... }:

with lib;
let
  cfg = config.modules.shell.git;
in {
  options.modules.shell.git = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.venikx = {
      home.packages = with pkgs; [
        gitAndTools.gh
        gitAndTools.git-open
        gitAndTools.diff-so-fancy
      ];

      xdg.configFile."git/config".source = "/etc/nixos/config/git/config";
      xdg.configFile."git/ignore".source = "/etc/nixos/config/git/ignore";
    };

    modules.shell.zsh.rcFiles = [ "/etc/nixos/config/git/aliases.zsh" ];
  };
}
