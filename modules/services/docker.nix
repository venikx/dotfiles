{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.services.docker;
in {
  options.modules.services.docker = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    users.users.venikx = { extraGroups = [ "docker" ]; };

    virtualisation = {
      docker = {
        enable = true;
        autoPrune.enable = true;
        enableOnBoot = false;
      };
    };
  };
}
