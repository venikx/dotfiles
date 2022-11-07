{ ... }:
{
  imports = [
    ./neovim.nix
  ];

  home.sessionVariables = {
    EDITOR = "nvim";
  };
}
