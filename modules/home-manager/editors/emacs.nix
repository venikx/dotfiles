{
  config,
  lib,
  pkgs,
  options,
  ...
}:

let
  inherit (lib) mkIf mkEnableOption optionals;
  cfg = config.modules.editors.emacs;
  emacsPackageList =
    epkgs:
    [
      epkgs.diminish
      # vim bindings
      epkgs.evil
      epkgs.evil-surround
      epkgs.evil-collection
      epkgs.evil-goggles
      epkgs.evil-commentary
      epkgs.evil-org
      # themes
      epkgs.doom-themes
      epkgs.all-the-icons # TODO
      # completion engine
      epkgs.vertico
      epkgs.vertico-prescient
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
      epkgs.ob-http
      epkgs.ob-graphql
      epkgs.ob-mermaid
      epkgs.ob-nix
      epkgs.ob-typescript
      epkgs.ob-go
      # epkgs.org-contacts #TODO link was removed from github
      epkgs.org-noter
      epkgs.denote
      epkgs.org-download
      epkgs.org-cliplink
      epkgs.org-ql
      # languages
      (epkgs.treesit-grammars.with-grammars (p: [
        p.tree-sitter-nix
        # Upstream's configurePhase picks the grammar subdir via `jq
        # 'select(.name == env.language)'`, but `language` isn't exported
        # to jq's environment under structuredAttrs, so the lookup always
        # misses and falls back to the first grammar (typescript) instead
        # of tsx. Bypass it by cd-ing into the right subdir directly.
        (p.tree-sitter-tsx.overrideAttrs (_old: {
          configurePhase = ''
            runHook preConfigure
            cd tsx
            runHook postConfigure
          '';
        }))
        p.tree-sitter-json
        p.tree-sitter-typescript
        p.tree-sitter-javascript
        p.tree-sitter-astro
        p.tree-sitter-html
        p.tree-sitter-css
        p.tree-sitter-go
        p.tree-sitter-gomod
        p.tree-sitter-gdscript
        p.tree-sitter-c
        p.tree-sitter-cpp
        p.tree-sitter-c-sharp
      ]))
      epkgs.nix-ts-mode
      epkgs.web-mode
      epkgs.astro-ts-mode
      epkgs.markdown-mode
      epkgs.gdscript-mode
      epkgs.glsl-mode
      epkgs.geiser
      # tools
      epkgs.osm
      epkgs.pdf-tools
      epkgs.elcord
      epkgs.nov
      epkgs.gnuplot
      epkgs.yasnippet
    ]
    ++ optionals cfg.ai.enable [
      epkgs.eca
    ];

  tex = pkgs.texliveSmall.withPackages (ps: [
    ps.wrapfig
    ps.ulem
    ps.capt-of
    ps.hyperref
    ps.amsmath
    ps.graphics
    ps.latex-bin
    ps.pgf
    ps.dvisvgm
  ]);

  emacsExtraPackages =
    with pkgs;
    [
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
      tex

      ### natural languages ###
      languagetool
      (aspellWithDicts (
        ds: with ds; [
          en
          en-computers
          en-science
        ]
      ))

      #### languages ####
      editorconfig-core-c
      typescript-language-server
      typescript
      vscode-langservers-extracted
      yaml-language-server
      dockerfile-language-server
      nixfmt
      shfmt
      ccls
      gcc
      clang
      glslang
      omnisharp-roslyn
      gomodifytags
      gopls
      gotests
      gore
      gotools
      rust-analyzer
      rustfmt
      rustc
      cargo
      go
      gopls
    ]
    ++ lib.optionals stdenv.isLinux [
      scrot # org-download
      ## pdf
      #qpdfview
      racket
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
  options.modules.editors.emacs.ai.enable = mkEnableOption "AI/LLM assistants (eca) in Emacs.";

  config = {
    home.packages = with pkgs; [
      emacsWithAdditionalPackages
    ];

    home.file.".config/emacs" = {
      source = ./emacs;
      recursive = true;
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
  };
}
