{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.hardware.audio;
in {
  options.modules.hardware.audio = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    sound.enable = true;
    sound.mediaKeys.enable = true;
    hardware.pulseaudio.enable = true;

    users.users.venikx = {
      extraGroups = [ "audio" ];
    };
  };
}
