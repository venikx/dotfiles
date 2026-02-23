{ pkgs, config, ... }:
{
  imports = [
    ./browsers.nix
    ./desktop-environment.nix
    ./emulators.nix
    ./music-production.nix
    ./obs.nix
    ./game-engines.nix
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
    (if pkgs.system == "x86_64-linux" then discord else legcord)
    (lib.mkIf (pkgs.system == "x86_64-linux") slack)
    # learning
    anki
    # media
    youtube-music
    evince
    calibre
    mpv-unwrapped
    vlc # in case mpv doesn't work
    # scripts
    (pkgs.writeShellScriptBin "scrcap" ''
      ${pkgs.maim}/bin/maim -s | ${pkgs.xclip}/bin/xclip -selection clipboard -t image/png
    '')
    (pkgs.writeShellScriptBin "define" ''
      if [ $# -gt 0 ]; then
        word="$*"
      else
        word=$(${pkgs.xclip}/bin/xclip -o -selection primary 2>/dev/null || ${pkgs.wl-clipboard}/bin/wl-paste 2>/dev/null)
      fi

      if [ -z "$word" ] || echo "$word" | grep -q '/'; then
        ${pkgs.libnotify}/bin/notify-send -h string:bgcolor:#bf616a -t 2000 "Invalid input."
        exit 0
      fi

      LANGS="en nl fi"
      lang=$(printf "%s\n" $LANGS | dmenu -c -i -l 5 -p "language")
      [ -z "$lang" ] && exit 0

      urlencode() {
        echo -n "$1" | ${pkgs.jq}/bin/jq -s -R -r @uri
      }
      encoded_word=$(urlencode "$word")

      query=$(${pkgs.curl}/bin/curl -s --connect-timeout 5 --max-time 10 "https://freedictionaryapi.com/api/v1/entries/$lang/$encoded_word")
      if [ $? -ne 0 ] || [ -z "$query" ]; then
        ${pkgs.libnotify}/bin/notify-send -h string:bgcolor:#bf616a -t 3000 "Connection error."
        exit 1
      fi

      if echo "$query" | ${pkgs.jq}/bin/jq -e '.entries == []' > /dev/n -cull; then
        ${pkgs.libnotify}/bin/notify-send -h string:bgcolor:#bf616a -t 3000 "No definitions found."
        exit 0
      fi

      def=$(echo "$query" | ${pkgs.jq}/bin/jq -r '
        .entries
        | group_by(.partOfSpeech)[]
        | .[0].partOfSpeech as $pos
        | "\($pos):\n" +
          (map(
             (.senses[0:2] // []) | map("  - " + (.definition // "")) | join("\n")
           ) | join("\n")) + "\n"
      ')

      ${pkgs.libnotify}/bin/notify-send -t 60000 "$word" "$def"
    '')
  ];
}
