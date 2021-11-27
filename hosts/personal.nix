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

}
