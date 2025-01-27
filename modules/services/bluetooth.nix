{ options, config, lib, pkgs, ... }:

with lib;
let
  hwCfg = config.modules.services;
  cfg = hwCfg.bluetooth;
in {
  options.modules.services.bluetooth = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    hardware.bluetooth = {
      enable = true;
      powerOnBoot = true;
      package = pkgs.bluez;
      settings = {
        General = {
          ControllerMode = "dual";
          MultiProfile = "multiple";
          Experimental = true;
        };
        Policy = { AutoEnable = true; };
      };
    };
    #services.blueman.enable = true;
  };
}
