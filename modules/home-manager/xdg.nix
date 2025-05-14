{ config, lib, pkgs, ... }:

with lib;
{
    programs.zsh.enable = true;
    xdg = mkMerge([
      {
        enable = true;
      }
      (mkIf pkgs.stdenv.isLinux {
        userDirs.enable = true;
        userDirs.download = "$HOME/dl";
        userDirs.documents = "$HOME/docs";
        userDirs.pictures = "$HOME/media/pics";
        userDirs.music = "$HOME/media/music";
        userDirs.videos = "$HOME/media/videos";
        userDirs.desktop = "$HOME/system/desktop";
        userDirs.publicShare = "$HOME/system/public";
        userDirs.templates = "$HOME/system/templates";
      })
    ]);

    home = {
      username = lib.mkDefault "venikx";
      homeDirectory = if pkgs.stdenv.isLinux then "/home/${config.home.username}" else "/Users/${config.home.username}";
      sessionPath = [ "$HOME/.local/bin" ];

      sessionVariables = {
        HISTFILE        = "${config.xdg.dataHome}/bash/history";
        INPUTRC         = "${config.xdg.configHome}/readline/inputrc";
        LESSKEY         = "${config.xdg.cacheHome}/less/lesskey";
        LESSHISTFILE    = "${config.xdg.cacheHome}/less/history";
        WGETRC          = "${config.xdg.configHome}/wgetrc";
      };
    };
}
