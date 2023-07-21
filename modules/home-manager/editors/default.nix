{ ... }:
{
  imports = [
    ./neovim.nix
    ./emacs.nix
  ];

  home.sessionVariables = {
    EDITOR = "nvim";
  };
}
