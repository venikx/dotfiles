{ ... }:

{
  imports = [
    ./fonts.nix
    ./xdg.nix
    ./xresources.nix
    #./desktop
    ./services
    ./cli
    ./editors
    ./dev
  ];
}
