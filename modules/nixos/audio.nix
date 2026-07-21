{
  config,
  lib,
  pkgs,
  ...
}:

lib.mkIf config.services.pipewire.enable {
  security.rtkit.enable = true;
  services.pipewire = {
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    wireplumber.enable = true;
  };

  environment.systemPackages = [
    pkgs.pavucontrol
    pkgs.alsa-utils
  ];
}
