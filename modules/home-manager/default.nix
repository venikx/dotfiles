{ config, lib, pkgs, ... }:

{
  users.users.venikx.shell = pkgs.zsh;
  programs.zsh.enable = true;

  environment = {
   variables = {
     XDG_CACHE_HOME  = "$HOME/.cache";
     XDG_CONFIG_HOME = "$HOME/.config";
     XDG_DATA_HOME   = "$HOME/.local/share";
     XDG_BIN_HOME    = "$HOME/.local/bin";
   };
  };

  home-manager.users.venikx.imports = [
    ./xdg.nix
    ./desktop
    ./cli
    ./editors
  ];

  nixpkgs.config = {
    allowUnfree = true;
  };
}
