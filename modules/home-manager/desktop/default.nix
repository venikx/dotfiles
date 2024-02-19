{ pkgs, ... }: {
  imports = [ ./music-production.nix ./obs.nix ];

  programs = { alacritty = { enable = true; }; };

  home.packages = with pkgs; [
    # communication
    (if pkgs.system == "x86_64-linux" then discord else armcord)
    (lib.mkIf (pkgs.system == "x86_64-linux") slack)
    # learning
    anki
    # media
    youtube-music
    evince
    calibre
    mpv-unwrapped
    vlc # in case mpv doesn't work
  ];
}
