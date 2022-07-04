{ config, lib, pkgs, options, ... }:

with lib;
let
  cfg = config.modules.shell.git;
  configDir = config.dotfiles.configDir;
in {
  options.modules.shell.git = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.venikx = {
      home.packages = with pkgs; [
        gitAndTools.gh
        gitAndTools.git-open
        gitAndTools.diff-so-fancy
      ];

      xdg.configFile."git/config".source = "${configDir}/git/config";
      xdg.configFile."git/ignore".source = "${configDir}/git/ignore";
    };

    modules.shell.zsh.rcFiles = [ "${configDir}/git/aliases.zsh" ];
  };
}
