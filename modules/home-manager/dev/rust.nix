{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.modules.dev.rust;
in {
  options.modules.dev.rust = { enable = mkEnableOption "Enable Rusty things"; };

  config = {
    home = {
      sessionVariables = {
        RUSTUP_HOME = "${config.xdg.dataHome}/rustup";
        CARGO_HOME = "${config.xdg.dataHome}/cargo";
      };
      sessionPath = [ "${config.home.sessionVariables.CARGO_HOME}/bin" ];

      packages = with pkgs; [ (mkIf cfg.enable rustup) ];
    };

  };
}
