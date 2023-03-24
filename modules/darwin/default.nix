{ ... }:
{
  imports =
    [
      ./gnupg.nix
      ./osx.nix
      ./yabai.nix
    ];

  homebrew = {
    brews = [
      {
         name = "emacs-mac";
         args = [ "with-modules" ];
      }
      "pngpaste" # needed to paste from clipboard to Emacs
    ];
    taps = [
      "railwaycat/emacsmacport"
    ];
    casks = [
      "google-chrome"
      "firefox"
      "steam"
    ];
    masApps = {
      #Xcode = 497799835;
      "1Password 7 - Password Manager" = 1333542190;
    };
  };
}
