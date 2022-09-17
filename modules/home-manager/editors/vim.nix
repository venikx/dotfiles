{ pkgs, ... }:
{
  home.packages = with pkgs; [
    editorconfig-core-c
    neovim
  ];

  programs.zsh.shellAliases = {
    vim = "nvim";
  };

  programs.git.ignores = [
    "*.swp"
    ".*.sw[a-z]"
    "*.un~"
    "Session.vim"
    ".netrwhist"
  ];
}
