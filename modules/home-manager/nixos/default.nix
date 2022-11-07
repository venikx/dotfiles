{ pkgs, ... }:
{
  imports =
    [
      ./bspwm.nix
      ./davinci-resolve.nix
    ];

  home.packages = with pkgs; [
    obsidian
  ];
}
