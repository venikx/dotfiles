{ config, lib, pkgs, options, emacs-overlay, ... }:

let
  inherit (lib) mkIf;
in {
  nixpkgs.overlays = [ emacs-overlay.overlay ];
  programs.emacs.enable = true;
  services.emacs = {
    enable = true;
    package = pkgs.emacs29;
  };

  home.packages = with pkgs; [
    #emacs

    ## Doom dependencies
    git
    (ripgrep.override {withPCRE2 = true;})
    gnutls  # for TLS connectivity

    ## Optional dependencies
    fd                  # faster projectile indexing
    imagemagick         # for image-dired
    zstd                # for undo-fu-session/undo-tree compression
    fd
    #(mkIf (config.programs.gnupg.agent.enable) pinentry-emacs)

    ## Module dependencies
    # :checkers spell
    (aspellWithDicts (ds: with ds; [ en en-computers en-science ]))
    # :checkers grammar
    languagetool
    # :tools editorconfig
    editorconfig-core-c
    # :tools lookup & :lang org +roam
    sqlite
    epdfview
    # :lang cc
    ccls
    # :lang json
    nodePackages.vscode-json-languageserver-bin
    # :lang javascript
    nodePackages.typescript-language-server
    nodePackages.typescript # NOTE(Kevin): Eglot can't resolve the globally installed package when typescript packages doesn't exist in root of project
    # :lang web
    nodePackages.vscode-css-languageserver-bin
    nodePackages.vscode-html-languageserver-bin
    # :lang rust
    rustfmt
    # :lang org +roam2 & org-roam-ui
    graphviz
    # :lang org +pandoc
    pandoc
    # :lang yaml
    nodePackages.yaml-language-server
    # :tools docker
    nodePackages.dockerfile-language-server-nodejs
  ];

  # Setting up Doom Emacs
  xdg.configFile."doom" = {
    source = ./doom;
    recursive = true;
  };
  home.sessionPath = [ "${config.xdg.configHome}/emacs/bin" ];

  # Emacs Utils
  programs.zsh.shellAliases = {
    e = "emacsclient -n";
    ediff = ''e --eval "(ediff-files \"$1\" \"$2\")"''; # used to be a function
  };
  programs.git.ignores = [ "*~" "*.*~" "\#*" ".\#*"];
  xsession.windowManager.bspwm.rules = {
    "Emacs" = {
      state = "tiled";
    };
    "Emacs:org*" = {
      state = "floating";
    };
    "Emacs:scratch" = {
      state = "floating";
    };
  };
}
