{ config, lib, pkgs, options, ... }:

with lib; {
  homebrew.brews = [ "pinentry-mac"];

  home-manager.users.kdebaerdemaeker = {
    home.packages = with pkgs; [ openssh libfido2 ];
    services.yubikey-agent = { enable = true; };
  };
}
