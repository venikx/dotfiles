{ pkgs, config, ... }: {
  imports = [
    ./browsers.nix
    ./desktop-environment.nix
    ./emulators.nix
    ./music-production.nix
    ./obs.nix
  ];

  programs = {
    alacritty = {
      enable = true;
      settings = {
        colors = {
          primary.background = "0x${config.colorScheme.palette.base00}";
          primary.foreground = "0x${config.colorScheme.palette.base07}";

          cursor.text = "0x${config.colorScheme.palette.base07}";
          cursor.cursor = "0x${config.colorScheme.palette.base07}";

          normal = {
            black = "0x${config.colorScheme.palette.base00}";
            white = "0x${config.colorScheme.palette.base07}";
            red = "0x${config.colorScheme.palette.base08}";
            yellow = "0x${config.colorScheme.palette.base09}";
            green = "0x${config.colorScheme.palette.base0B}";
            blue = "0x${config.colorScheme.palette.base0D}";
            magenta = "0x${config.colorScheme.palette.base0E}";
            cyan = "0x${config.colorScheme.palette.base0C}";
          };
        };
      };
    };
  };

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
