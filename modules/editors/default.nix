{ pkgs, home-manager, lib, ... }:

with lib;
{
  imports = [
    ./emacs.nix
    ./vim.nix
  ];

  home-manager.users.venikx = {
    home.sessionVariables = {
      EDITOR = "nvim";
    };
  };
}
