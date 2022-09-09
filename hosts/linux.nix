{ options, config, lib, pkgs, ... }:

with lib;
{
  time.timeZone = mkDefault "Europe/Helsinki";
  i18n.defaultLocale = mkDefault "en_US.UTF-8";
  # Mainly for redshift
  location = {
    # Helsinki
    latitude = 60.192059;
    longitude = 24.945831;
  };

  config.user = {
      isNormalUser = true;
      home = "/home/venikx";
      extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
      # Define a user account. Don't forget to set a password with ‘passwd’.
      initialPassword = "v3nikx";
  };

  # Move ~/.Xauthority out of $HOME (setting XAUTHORITY early isn't enough)
  environment.extraInit = ''
     export XAUTHORITY=/tmp/Xauthority
     [ -e ~/.Xauthority ] && mv -f ~/.Xauthority "$XAUTHORITY"
  '';
}
