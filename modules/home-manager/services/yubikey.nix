{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [ openssh libfido2 ];
  services.yubikey-agent = { enable = true; };
}
