{ config, nix-colors, ... }:

{
  #programs.autorandr = {
  #  enable = true;
  #  profiles = {
  #    "default" = {
  #      fingerprint = {
  #        eDP =
  #          "00ffffffffffff003870500000000000011d0104a51f117803c455a15651a1280d505400000001010101010101010101010101010101028380047138ca403020350035ae1000001a814180047138ca403020350035ae1000001a000000fd0030789a9a22010a202020202020000000fe004c4d3134304c462d314630310a0044";
  #      };
  #      config = {
  #        eDP = {
  #          enable = true;
  #          primary = true;
  #          rate = "120.00";
  #          mode = "1920x1080";
  #          dpi = 100;
  #        };
  #      };
  #    };
  #  };
  #};

  colorScheme = nix-colors.colorSchemes.tokyo-city-terminal-dark;
  home.stateVersion = "24.11";
}
