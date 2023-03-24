{ pkgs, ... }:
{
  imports = [
    ./terminal.nix
  ];

  home.packages = with pkgs; [
    # communication
    discord
    slack
    teams
  ];
}
