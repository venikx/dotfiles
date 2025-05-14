{ config, lib, pkgs, options, emacs-overlay, ... }:

let inherit (lib) mkIf;
in {
  #programs.emacs.enable = true;
  #services.emacs = { enable = true; };

  home.packages = with pkgs; [
    #emacs-unstable
    ((emacsPackagesFor emacs).emacsWithPackages (epkgs: [
      epkgs.vterm
      epkgs.pdf-tools
      epkgs.all-the-icons
      epkgs.treesit-grammars.with-all-grammars
    ]))

    ## Doom dependencies
    git
    (ripgrep.override { withPCRE2 = true; })
    gnutls # for TLS connectivity
    gnuplot

    ## Optional dependencies
    fd # faster projectile indexing
    imagemagick # for image-dired
    zstd # for undo-fu-session/undo-tree compression
    fd
    #(mkIf (config.programs.gnupg.agent.enable) pinentry-emacs)

    ## Vterm
    cmake
    gnumake
    ## Module dependencies
    # :checkers spell
    (aspellWithDicts (ds: with ds; [ en en-computers en-science ]))
    # :checkers grammar
    languagetool
    # :tools editorconfig
    editorconfig-core-c
    # :tools lookup & :lang org +roam
    sqlite
    # lsp
    nodejs
    # cc
    ccls
    glslang
    # c#
    omnisharp-roslyn
    csharpier
    # markdown
    python311Packages.grip
    # nix
    nixfmt
    # sh
    shfmt
    # javascript
    nodePackages.typescript-language-server
    nodePackages.typescript # NOTE(Kevin): Eglot can't resolve the globally installed package when typescript packages doesn't exist in root of project
    # web
    nodePackages.vscode-langservers-extracted
    nodePackages.stylelint
    nodePackages.js-beautify
    html-tidy
    # go
    gomodifytags
    gopls
    gotests
    gore
    gotools
    # rust
    rust-analyzer
    rustfmt
    rustc
    cargo
    # org +roam2 & org-roam-ui
    graphviz
    pandoc
    # yaml
    nodePackages.yaml-language-server
    # docker
    nodePackages.dockerfile-language-server-nodejs
  ] ++ lib.optionals stdenv.isLinux [
    # img capturing from emacs
    scrot
    # pdf
    qpdfview
    # racket
    racket
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
  programs.git.ignores = [ "*~" "*.*~" "#*" ".#*" ];
  xsession.windowManager.bspwm.rules = {
    "Emacs" = { state = "tiled"; };
    "Emacs:org*" = { state = "floating"; };
    "Emacs:scratch" = { state = "floating"; };
  };
}

#   fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];
# TODO(Kevin): Install doom emacs as an emacs-overlay
#system.activationScripts = mkIf cfg.doom.enable {
#  installDoomEmacs = ''
#    if [ ! -d "$XDG_CONFIG_HOME/emacs" ]; then
#       git clone --depth=1 --single-branch https://github.com/doomemacs/doomemacs "$XDG_CONFIG_HOME/emacs"
#    fi
#  '';
#};
