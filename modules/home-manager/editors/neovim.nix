{ pkgs, ... }:
{
  home.packages = with pkgs; [
    editorconfig-core-c
  ];

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
  };

  programs.git.ignores = [
    "*.swp"
    ".*.sw[a-z]"
    "*.un~"
    "Session.vim"
    ".netrwhist"
  ];
}
