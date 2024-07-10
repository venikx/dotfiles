{ config, lib, pkgs, ... }:

{
  fonts.fontconfig = {
    enable = true;
    defaultFonts = {
      sansSerif = [ "Fira Sans" ];
      monospace = [ "Fira Code" ];
    };
  };

  home.packages = with pkgs; [
    fira-code
    fira-code-symbols
    siji
    barlow
    jetbrains-mono
    font-awesome
  ];
}
