{ config, lib, pkgs, ... }:

{
  config = {
    home = {
      sessionVariables = {
        DOCKER_CONFIG = "${config.xdg.configHome}/docker";
        MACHINE_STORAGE_PATH = "${config.xdg.dataHome}/docker/machine";
      };

      packages = with pkgs; [ docker docker-compose ];
    };
  };

}
