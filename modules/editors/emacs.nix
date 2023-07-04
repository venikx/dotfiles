{ config, lib, pkgs, options, emacs-overlay, ... }:

with lib;
let
  cfg = config.modules.editors.emacs;
  configDir = config.dotfiles.configDir;
in {
  options.modules.editors.emacs = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };

    doom = {
      enable = mkOption {
        type = bool;
        default = true;
      };
    } ;
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [ emacs-overlay.overlay ];

    fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];
    environment.systemPackages = with pkgs; [
      (mkIf pkgs.stdenv.isLinux emacs-unstable) # emacs on mac handled by brew

      ## Doom dependencies
      git
      (ripgrep.override {withPCRE2 = true;})
      gnutls              # for TLS connectivity

      ## Optional dependencies
      fd                  # faster projectile indexing
      imagemagick         # for image-dired
      zstd                # for undo-fu-session/undo-tree compression
      fd
      (mkIf (config.programs.gnupg.agent.enable) pinentry-emacs)

      ## Module dependencies
      # :checkers spell
      (aspellWithDicts (ds: with ds; [ en en-computers en-science ]))
      # :checkers grammar
      (mkIf pkgs.stdenv.isLinux languagetool)
      # :tools editorconfig
      editorconfig-core-c
      # :tools lookup & :lang org +roam
      sqlite
      # :lang latex & :lang org (latex previews)
      texlive.combined.scheme-full
      (mkIf pkgs.stdenv.isLinux epdfview)
      # :lang beancount
      beancount
      fava  # HACK Momentarily broken on nixos-unstable
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

    home-manager.users.venikx = mkIf cfg.doom.enable {
      xdg.configFile."doom" = {
        source = "${configDir}/doom";
        # link recursively so other modules can link files in their folders
        recursive = true;
      };

      home.sessionPath = [ "$XDG_CONFIG_HOME/emacs/bin" ]; 
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
    };

    # TODO(Kevin): Install doom emacs as an emacs-overlay
    #system.activationScripts = mkIf cfg.doom.enable {
    #  installDoomEmacs = ''
    #    if [ ! -d "$XDG_CONFIG_HOME/emacs" ]; then
    #       git clone --depth=1 --single-branch https://github.com/doomemacs/doomemacs "$XDG_CONFIG_HOME/emacs"
    #    fi
    #  '';
    #};
  };
}
