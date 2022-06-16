{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.gaming.epic;
    unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
in {
  options.modules.desktop.gaming.epic = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    hardware = {
      opengl.enable = true;
      opengl.driSupport32Bit  = true;
      pulseaudio.support32Bit = config.hardware.pulseaudio.enable;
    };

    home-manager.users.venikx = {
      home.packages = with pkgs; [
        lutris
        unstable.legendary-gl
        unstable.heroic
      ];
    };
  };
}
