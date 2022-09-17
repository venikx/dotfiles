{ config, lib, ... }:

with lib;
{
    programs.zsh.enable = true;
    xdg = mkMerge([
      {
        enable = true;
        # home-manager expects these to be paths, and $HOME isn't
        # cutting for current unknown reasons
        configHome = "${config.users.users.venikx.home}/.config";
        dataHome = "${config.users.users.venikx.home}/.local/share";
        cacheHome  = "${config.users.users.venikx.home}/.cache";
      }
      (mkIf pkgs.stdenv.isLinux {
        userDirs.enable = true;
        userDirs.download = "$HOME/dl";
        userDirs.documents = "$HOME/dl/docs";
        userDirs.pictures = "$HOME/media/pics";
        userDirs.music = "$HOME/media/music";
        userDirs.videos = "$HOME/media/videos";
        userDirs.desktop = "$HOME/system/desktop";
        userDirs.publicShare = "$HOME/system/public";
        userDirs.templates = "$HOME/system/templates";
      })
    ]);

    home = {
      homeDirectory = config.users.users.venikx.home;
      username = config.users.users.venikx.name;
      sessionPath = [ config.environment.variables.XDG_BIN_HOME ];

      sessionVariables = {
        HISTFILE        = "${config.environment.variables.XDG_DATA_HOME}/bash/history";
        INPUTRC         = "${config.environment.variables.XDG_CONFIG_HOME}/readline/inputrc";
        LESSKEY         = "${config.environment.variables.XDG_CACHE_HOME}/less/lesskey";
        LESSHISTFILE    = "${config.environment.variables.XDG_CACHE_HOME}/less/history";
        WGETRC          = "${config.environment.variables.XDG_CONFIG_HOME}/wgetrc";
      };
    };
}
