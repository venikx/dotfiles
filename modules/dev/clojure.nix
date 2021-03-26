{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.dev.clojure;
in {
  options.modules.dev.clojure = with types; {
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
    (mkIf cfg.binaries.enable {
      home-manager.users.venikx = {
        home.packages = with pkgs; [
          clojure
          leiningen
        ];
      };
    })
  ]);
}
