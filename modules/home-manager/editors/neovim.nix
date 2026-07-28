{ pkgs, ... }:
{
  home.packages = with pkgs; [
    editorconfig-core-c
  ];

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    initLua = builtins.readFile ./nvim/init.lua;
  };

  programs.git.ignores = [
    "*.swp"
    ".*.sw[a-z]"
    "*.un~"
    "Session.vim"
    ".netrwhist"
  ];
}
