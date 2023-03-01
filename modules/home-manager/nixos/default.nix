{ pkgs, ... }:
{
  imports =
    [
      ./bspwm.nix
    ];

  home.packages = with pkgs; [
    # video editing
    davinci-resolve # only mov is supported, maybe this is fixed in v18?
    ffmpeg_5
  ];
}
