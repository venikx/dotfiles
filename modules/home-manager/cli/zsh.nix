{ config, lib, pkgs, ... }:

{
  home.sessionVariables = {
    ZDOTDIR = "${config.xdg.configHome}/zsh";
    ZSH_CACHE = "${config.xdg.cacheHome}/zsh";
  };

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    syntaxHighlighting = { enable = true; };
    enableVteIntegration = true;
    autocd = true;
    cdpath = [ "~/code" ];
    defaultKeymap = "viins";
    #dirHashes = {};
    dotDir = ".config/zsh";
    historySubstringSearch.enable = true;
    history = {
      expireDuplicatesFirst = true;
      extended = true;
      ignoreSpace = true;
      path = "${config.xdg.dataHome}/zsh/zsh_history";
    };

    initExtraBeforeCompInit = ''
      bindkey -M viins '^[[Z' reverse-menu-complete

      zman() {
        PAGER="less -g -I -s '+/^       "$1"'" man zshall;
      }

      source ${./prompt.zsh}
    '';
    shellAliases = {
      mkdir = "mkdir -p";
      wget = "wget -c";
      gurl = "curl --compressed";
      lsd = "eza";
    };
  };

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
    defaultOptions = [ "--reverse" "--ansi" ];
    defaultCommand = "fd .";
    fileWidgetCommand = "fd .";
    changeDirWidgetCommand = "fd --type d . $HOME";
  };

  programs.eza = { enable = true; };

  home.packages = with pkgs; [ bat fd tldr ];
}
