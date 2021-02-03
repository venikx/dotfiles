{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.editors;
in {
  imports = [
    ./emacs.nix
    ./vim.nix
  ];
  options.modules.editors = with types; {
    default = mkOption {
      type = nullOr str;
      default = "vim";
    };
  };

  config = mkIf (cfg.default != null) {
    env.EDITOR = cfg.default;
  };
}
