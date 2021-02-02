{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.dev.node;
in {
  options.modules.dev.node = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };

    binaries.enable = mkOption {
      type = bool;
      default = false;
    };
  };
  
  config = mkIf cfg.enable (mkMerge [
    {
      env.NPM_CONFIG_USERCONFIG = "$XDG_CONFIG_HOME/npm/config";
      env.NPM_CONFIG_CACHE      = "$XDG_CACHE_HOME/npm";
      env.NPM_CONFIG_TMP        = "$XDG_RUNTIME_DIR/npm";
      env.NPM_CONFIG_PREFIX     = "$XDG_CACHE_HOME/npm";
      env.NODE_REPL_HISTORY     = "$XDG_CACHE_HOME/node/repl_history";

      home-manager.users.venikx = {
        xdg.configFile."npm/config".text = ''
        cache=$XDG_CACHE_HOME/npm
        prefix=$XDG_DATA_HOME/npm
      '';
      };
    }

    (mkIf cfg.binaries.enable {
      env.PATH = [ "$(yarn global bin)" ];
      environment.shellAliases = {
        ya = "yarn";
      };

      home-manager.users.venikx = {
        home.packages = with pkgs; [
          nodejs
          yarn
        ];
      };
    })
  ]);
}
