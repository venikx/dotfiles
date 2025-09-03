{
  config,
  lib,
  pkgs,
  options,
  emacs-overlay,
  ...
}:

let
  inherit (lib) mkIf;
  emacsPackageList = epkgs: [
    # vim bindings
    epkgs.evil
    epkgs.evil-surround
    epkgs.evil-collection
    epkgs.evil-goggles
    epkgs.evil-commentary
    # themes
    epkgs.doom-themes
    epkgs.all-the-icons # TODO
    # completion engine
    epkgs.vertico
    epkgs.marginalia
    epkgs.orderless
    epkgs.consult
    epkgs.corfu
    # git
    epkgs.magit
    epkgs.magit-todos
    # shells
    epkgs.vterm # TODO
    epkgs.envrc
    # formatting
    epkgs.apheleia
    epkgs.editorconfig
    # org
    epkgs.org-contrib
    # epkgs.org-contacts #TODO link was removed from github
    epkgs.org-roam
    epkgs.org-download
    epkgs.org-ql
    # languages
    epkgs.treesit-grammars.with-all-grammars
    epkgs.web-mode
    epkgs.nix-ts-mode
    epkgs.markdown-mode
    # LLM
    epkgs.copilot # TODO
    # tools
    epkgs.osm
    epkgs.pdf-tools # TODO
    epkgs.elcord
    epkgs.nov
    epkgs.gnuplot
  ];

  emacsExtraPackages = with pkgs; [
    #### basic ####
    gnutls
    git
    zstd
    # required: consult
    (ripgrep.override { withPCRE2 = true; })
    fd
    # required: vterm
    cmake
    gnumake

    #### org-mode ####
    sqlite
    gnuplot # nutrition.el
    pandoc
    graphviz

    #### languages ####
    editorconfig-core-c
    nodePackages.typescript-language-server
    nodePackages.typescript
    nodePackages.vscode-langservers-extracted
    nodePackages.stylelint # TODO
    nodePackages.yaml-language-server
    nodePackages.dockerfile-language-server-nodejs
    nixfmt
    shfmt
  ];

  emacsWithAdditionalPackages = pkgs.symlinkJoin {
    name = "emacs-with-additional-packages";
    paths = [
      ((pkgs.emacsPackagesFor pkgs.emacs).emacsWithPackages emacsPackageList)
    ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      for prog in emacs emacsclient; do
        if [ -f "$out/bin/$prog" ]; then
          wrapProgram "$out/bin/$prog" --prefix PATH : ${pkgs.lib.makeBinPath emacsExtraPackages}
        fi
      done
    '';
  };
in
{
  #programs.emacs.enable = true;
  #services.emacs = { enable = true; };

  home.packages =
    with pkgs;
    [
      emacsWithAdditionalPackages
      ### Optional dependencies
      #imagemagick # for image-dired
      ##(mkIf (config.programs.gnupg.agent.enable) pinentry-emacs)

      ### Module dependencies
      ## :checkers spell
      #(aspellWithDicts (ds: with ds; [ en en-computers en-science ]))
      ## :checkers grammar
      #languagetool
      ## :tools editorconfig
      ## lsp
      #nodejs
      ## cc
      #ccls
      #glslang
      ## c#
      #omnisharp-roslyn
      ## go
      #gomodifytags
      #gopls
      #gotests
      #gore
      #gotools
      ## rust
      #rust-analyzer
      #rustfmt
      #rustc
      #cargo
    ]
    ++ lib.optionals stdenv.isLinux [
      ## img capturing from emacs
      #scrot
      ## pdf
      #qpdfview
      ## racket
      #racket
    ];

  xdg.configFile."emacs/config.org" = {
    source = ./emacs/config.org;
  };

  # Emacs Utils
  programs.zsh.shellAliases = {
    e = "emacsclient -n";
    ediff = ''e --eval "(ediff-files \"$1\" \"$2\")"''; # used to be a function
  };
  programs.git.ignores = [
    "*~"
    "*.*~"
    "#*"
    ".#*"
  ];
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
