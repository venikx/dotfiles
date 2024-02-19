{ pkgs, ... }: {
  imports = [ ./terminal.nix ./obs.nix ];

  home.packages = with pkgs; [
    # communication
    (if pkgs.system == "x86_64-linux" then discord else armcord)
    (lib.mkIf (pkgs.system == "x86_64-linux") slack)
    # learning
    anki
    # media
    youtube-music
  ];
}
