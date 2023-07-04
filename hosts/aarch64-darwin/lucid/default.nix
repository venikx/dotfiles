{ pkgs, ... }:

{
  users.users.venikx = {
    name = "venikx";
    description = "Kevin De Baerdemaeker";
    home = "/Users/venikx";
    shell = pkgs.zsh;
  }

  environment.systemPath = [ "/opt/homebrew/bin" ];
  system.stateVersion = 4;
}
