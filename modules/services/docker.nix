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
    env.DOCKER_CONFIG = "$XDG_CONFIG_HOME/docker";
    env.MACHINE_STORAGE_PATH = "$XDG_DATA_HOME/docker/machine";

    home-manager.users.venikx = {
      home.packages = with pkgs; [
        docker
        docker-compose
      ];
    };
    users.users.venikx = {
      extraGroups = [ "docker" ];
    };

    virtualisation = {
      docker = {
        enable = true;
        autoPrune.enable = true;
        enableOnBoot = false;
      };
    };
  };
}
