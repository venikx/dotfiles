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

      programs.zsh.shellAliases = {
        git  = ''noglob git'';
        ga   = ''git add'';
        gap  = ''git add --patch'';
        gb   = ''git branch -av'';
        gc   = ''git commit'';
        gcm  = ''git commit -m'';
        gca  = ''git commit --amend'';
        gl   = ''git log --graph --pretty="format:%C(yellow)%h%Creset %C(red)%G?%Creset%C(green)%d%Creset %s %Cblue(%cr) %C(bold blue)<%aN>%Creset"'';
        gll  = ''git log --pretty="format:%C(yellow)%h%Creset %C(red)%G?%Creset%C(green)%d%Creset %s %Cblue(%cr) %C(bold blue)<%aN>%Creset"'';
        gL   = ''gl --stat'';
        gss  = ''git status'';
        gs   = ''git status --short .'';
        gst  = ''git stash'';
        gr   = ''git reset HEAD'';
        grv  = ''git rev-parse'';
      };
    };
  };
}
