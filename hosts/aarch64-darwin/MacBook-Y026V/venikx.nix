{ lib, nix-colors, ... }:

{
  home.stateVersion = "24.11";
  home.username = lib.mkForce "kdebaerdemaeker";
  programs.git.userEmail = lib.mkForce "kdebaerdemaeker@netflix.com";

  services.syncthing.settings = {
    devices = {
      "earth-nixos" = {
        id = "QUIA5TB-Q62NJOZ-NXNZPLY-6YXHXEJ-W5A6YMS-LTY2PXH-AZE5YHQ-22SHBQL";
      };
      "air-nixos" = {
        id = "WK7RS2C-362VDSU-6AADX3Q-AFTADBL-PNY3KJO-ALQ6HYO-S6MQMOU-6MSYYAR";
      };
    };
    folders = { "org" = { devices = [ "earth-nixos" "air-nixos" ]; }; };
  };

  home.sessionVariables = {
    DEVELOPER_DIR = "/Applications/Xcode.app/Contents/Developer";
  };

  colorScheme = nix-colors.colorSchemes.tokyo-city-terminal-dark;
  home.sessionPath = lib.mkAfter [
    "$HOME/.sdkman/candidates/java/current/bin"
    "$HOME/.dotnet/tools"
    "$HOME/Library/Android/sdk/platform-tools"
    "/usr/local/share/dotnet"

    "/System/Cryptexes/App/usr/bin"
    "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/local/bin"
    "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin"
    "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin"
    "/Library/Apple/usr/bin"
    "/usr/local/munki"
    "/opt/nflx"
    "/opt/nflx/bin"
  ];
}
