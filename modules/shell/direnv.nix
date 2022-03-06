{ config, lib, pkgs, options, ... }:

with lib;
let
  cfg = config.modules.shell.direnv;
in {
  options.modules.shell.direnv = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.venikx = {
      programs.direnv.enable = true;
      programs.direnv.nix-direnv.enable = true;
    };
    modules.shell.zsh.rcInit = ''eval "$(direnv hook zsh)"'';
  };
}
