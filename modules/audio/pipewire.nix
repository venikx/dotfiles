{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkEnableOption mkForce;
  cfg = config.modules.audio.pipewire;
in {
  options.modules.audio.pipewire = {
    enable = mkEnableOption "Enable sound, via pipewire";
  };

  config = mkIf cfg.enable {
    hardware.pulseaudio.enable = mkForce false;

    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      wireplumber.enable = true;
    };

    environment.systemPackages =
      builtins.attrValues { inherit (pkgs) pavucontrol alsa-utils; };
  };
}
