{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.dev.rust;
in {
  options.modules.dev.rust = with types; {
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
      env.RUSTUP_HOME = "$XDG_DATA_HOME/rustup";
      env.CARGO_HOME = "$XDG_DATA_HOME/cargo";
      env.PATH = [ "$CARGO_HOME/bin" ];
    }

    (mkIf cfg.binaries.enable {
      env.PATH = [ "$(yarn global bin)" ];
      environment.shellAliases = {
        rs  = "rustc";
        rsp = "rustup";
        ca  = "cargo";
      };

      home-manager.users.venikx = {
        home.packages = with pkgs; [
          rustup
        ];
      };
    })
  ]);
}
