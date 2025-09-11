{
  config,
  lib,
  pkgs,
  ...
}:

{
  fonts.fontconfig = {
    enable = true;
    defaultFonts = {
      monospace = [ "Iosevka" ];
      sansSerif = [ "Iosevka" ];
      serif = [ "Iosevka" ];
    };
  };

  home.packages = with pkgs; [
    iosevka
    fira-code
    fira-code-symbols
    emacs-all-the-icons-fonts
  ];
}
