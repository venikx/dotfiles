{ pkgs, lib, ... }: {
  imports = [
    ./gnupg.nix

    ../common.nix
    ../audio
    ../desktop
    ../services
  ];

  time.timeZone = lib.mkDefault "Europe/Helsinki";
  i18n.defaultLocale = lib.mkDefault "en_US.UTF-8";

  location = {
    # Helsinki
    latitude = 60.192059;
    longitude = 24.945831;
  };

  # Move ~/.Xauthority out of $HOME (setting XAUTHORITY early isn't enough)
  environment.extraInit = ''
    export XAUTHORITY=/tmp/Xauthority
    [ -e ~/.Xauthority ] && mv -f ~/.Xauthority "$XAUTHORITY"
  '';

  hardware.keyboard.zsa.enable = true;
  environment.systemPackages = with pkgs; [ wally-cli ];
}
