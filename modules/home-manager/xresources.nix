{ config, lib, pkgs, ... }:

{
  xresources.properties = {
    "*.foreground" = "#${config.colorScheme.palette.base0F}";
    "*.background" = "#${config.colorScheme.palette.base07}";
    "*.cursorColor" = "#${config.colorScheme.palette.base07}";

    "*.color0" = "#${config.colorScheme.palette.base00}";
    "*.color1" = "#${config.colorScheme.palette.base01}";
    "*.color2" = "#${config.colorScheme.palette.base02}";
    "*.color3" = "#${config.colorScheme.palette.base03}";
    "*.color4" = "#${config.colorScheme.palette.base04}";
    "*.color5" = "#${config.colorScheme.palette.base05}";
    "*.color6" = "#${config.colorScheme.palette.base06}";
    "*.color7" = "#${config.colorScheme.palette.base07}";
    "*.color8" = "#${config.colorScheme.palette.base08}";
    "*.color9" = "#${config.colorScheme.palette.base09}";
    "*.color10" = "#${config.colorScheme.palette.base0A}";
    "*.color11" = "#${config.colorScheme.palette.base0B}";
    "*.color12" = "#${config.colorScheme.palette.base0C}";
    "*.color13" = "#${config.colorScheme.palette.base0D}";
    "*.color14" = "#${config.colorScheme.palette.base0E}";
    "*.color15" = "#${config.colorScheme.palette.base0F}";

    "scratch.font" = "Fira Code:pixelsize=16";
    "st.font" = "Fira Code:pixelsize=12";
    "URxvt*depth" = 32;
    "URxvt*.background" = "#${config.colorScheme.palette.base00}";
    "URxvt*.borderless" = 1;
    "URxvt*.buffered" = true;
    "URxvt*.cursorBlink" = true;
    "URxvt*.font" = "xft:Fira Code:pixelsize=12";
    "URxvt*.internalBorder" = 10;
    "URxvt*.letterSpace" = 0;
    "URxvt*.lineSpace" = 0;
    "URxvt*.loginShell" = false;
    "URxvt*.matcher.button" = 1;
    "URxvt*.matcher.rend.0" = "Uline Bold fg5";
    "URxvt*.saveLines" = 5000;
    "URxvt*.scrollBar" = false;
    "URxvt*.underlineColor" = "#${config.colorScheme.palette.base0F}";
    "URxvt.clipboard.autocopy" = true;
    "URxvt.iso14755" = false;
    "URxvt.iso14755_52" = false;
    "URxvt.perl-ext-common" = "default,matcher";

    "dmenu.font" = "xft:Fira Code:pixelsize=12";
    "dmenu.background" = "#${config.colorScheme.palette.base00}";
    "dmenu.foreground" = "#${config.colorScheme.palette.base07}";
    "dmenu.selbackground" = "#${config.colorScheme.palette.base00}";
    "dmenu.selforeground" = "#${config.colorScheme.palette.base07}";
  };
}
