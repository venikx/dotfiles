{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.modules.dev.nodejs;
in {
  options.modules.dev.nodejs = { enable = mkEnableOption "Enable nodejs"; };

  config = {
    xdg.configFile."npm/config".text = ''
      cache=${config.xdg.cacheHome}/npm
      prefix=${config.xdg.dataHome}/npm
      tmp=''${XDG_RUNTIME_DIR}/npm
    '';

    home.sessionVariables = {
      NPM_CONFIG_USERCONFIG = "${config.xdg.configHome}/npm/config";
      NPM_CONFIG_CACHE = "${config.xdg.cacheHome}/npm";
      NPM_CONFIG_TMP = "$XDG_RUNTIME_DIR/npm";
      NPM_CONFIG_PREFIX = "${config.xdg.cacheHome}/npm";
      NODE_REPL_HISTORY = "${config.xdg.cacheHome}/node/repl_history";
    };

    home.packages = with pkgs; [ (mkIf cfg.enable nodejs) ];
  };
}
