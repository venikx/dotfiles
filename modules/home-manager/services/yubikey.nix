{
  config,
  lib,
  pkgs,
  ...
}:

{
  home.packages = with pkgs; [
    openssh
    libfido2
    yubikey-manager
  ];
  services.yubikey-agent.enable = true;
}
