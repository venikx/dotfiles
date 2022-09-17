{ ... }:
{
  imports = [
    ./vim.nix
  ];

  home.sessionVariables = {
    EDITOR = "nvim";
  };
}
