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
      #emacs fast
      emacsPgtk # ok for now
      #emacsUnstable #toTest
      #emacsPgtkNativeComp # slow as balls

      ## Doom dependencies
      git
      (ripgrep.override {withPCRE2 = true;})
      gnutls              # for TLS connectivity

      ## Optional dependencies
      fd                  # faster projectile indexing
      imagemagick         # for image-dired
      zstd                # for undo-fu-session/undo-tree compression
      fd
      (mkIf (config.programs.gnupg.agent.enable) pinentry_emacs)

      ## Module dependencies
      # checkers
      (aspellWithDicts (ds: with ds; [
        en en-computers en-science
      ]))
      # c programming
      ccls
      editorconfig-core-c 
      # javascript
      nodePackages.typescript-language-server
      nodePackages.vscode-css-languageserver-bin
      nodePackages.vscode-html-languageserver-bin
      nodePackages.vue-language-server
      nodePackages.vscode-json-languageserver-bin
      # rust
      rustfmt
      # org-mode
      sqlite
      texlive.combined.scheme-medium
      plantuml
      graphviz
      pandoc
      # docker
      nodePackages.dockerfile-language-server-nodejs
      # accounting
      ledger
      hledger
      beancount
      # nodePackages.beancount-langserver
    ];

    home-manager.users.venikx = mkIf cfg.doom.enable {
      xdg.configFile."doom" = {
        source = "${configDir}/doom";
        # link recursively so other modules can link files in their folders
        recursive = true;
      };

      home.sessionPath = [ "$XDG_CONFIG_HOME/emacs/bin" ]; 
      programs.zsh = {
        shellAliases = {
          e = "emacsclient -n";
          ediff = ''e --eval "(ediff-files \"$1\" \"$2\")"''; # used to be a function
        };
      };
      programs.git.ignores = [ "*~" "*.*~" "\#*" ".\#*"];
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
