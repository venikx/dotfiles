{ config, lib, pkgs, options, ... }:

with lib;
let
  cfg = config.modules.editors.emacs;
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
    nixpkgs.overlays = [ 
      (import (builtins.fetchTarball https://github.com/nix-community/emacs-overlay/archive/master.tar.gz))
    ];

    fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];
    environment.systemPackages = with pkgs; [
      ## Emacs itself
      binutils       # native-comp needs 'as', provided by this
      # Not possible due to some changes on HEAD and mismatched doom-emacs,
      # better use Emacs 27.0
      # emacsPgtkGcc   # 28 + pgtk + native-comp
      emacsUnstable   # 28 + pgtk + native-comp

      ## Doom dependencies
      git
      (ripgrep.override {withPCRE2 = true;})
      gnutls              # for TLS connectivity

      ## Optional dependencies
      fd                  # faster projectile indexing
      imagemagick         # for image-dired
      zstd                # for undo-fu-session/undo-tree compression
      fd
      clang

      ## Module dependencies
      # checkers
      (aspellWithDicts (ds: with ds; [
        en en-computers en-science
      ]))
      # c programming
      ccls
      editorconfig-core-c 
      # javascript
      nodePackages.typescript
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
      #docker
      nodePackages.dockerfile-language-server-nodejs
    ];

    env.PATH = [ "$XDG_CONFIG_HOME/emacs/bin" ];
    modules.shell.zsh.rcFiles = [ "/etc/nixos/config/emacs/aliases.zsh" ];
    
    home-manager.users.venikx = mkIf cfg.doom.enable {
      xdg.configFile."doom" = {
        source = "/etc/nixos/config/doom";
        # link recursively so other modules can link files in their folders
        recursive = true;
      };
    };
  };
}
