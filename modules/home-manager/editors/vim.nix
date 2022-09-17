{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.editors.vim;
in {
  options.modules.editors.vim = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.venikx = {
      home.packages = with pkgs; [
        editorconfig-core-c
        neovim
      ];

      programs.zsh.shellAliases = {
        vim = "nvim";
      };

      programs.git.ignores = [
        "*.swp"
        ".*.sw[a-z]"
        "*.un~"
        "Session.vim"
        ".netrwhist"
      ];
    };
  };
}
