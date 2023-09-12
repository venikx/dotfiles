{ pkgs, ... }:
{
  imports = [
    ./terminal.nix
    ./obs.nix
  ];

  home.packages = with pkgs; [
    # communication
    discord
    #slack
    #teams
    # learning
    anki
  ];
}
