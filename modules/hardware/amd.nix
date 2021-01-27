{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.hardware.amd;
in {
  options.modules.hardware.amd = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    services.xserver.videoDrivers = [ "amdgpu" ];
  };
}
