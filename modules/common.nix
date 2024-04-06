{ pkgs, emacs-overlay, my-handmade-hero ... }:

{
  programs.zsh.enable = true;

  nixpkgs.overlays = [ emacs-overlay.overlay ];
  environment = {
    variables = {
      # TODO(Kevin): I think this is sometimes not setting up correctly?
      # race condition perhaps?
      XDG_CACHE_HOME = "$HOME/.cache";
      XDG_CONFIG_HOME = "$HOME/.config";
      XDG_DATA_HOME = "$HOME/.local/share";
      XDG_BIN_HOME = "$HOME/.local/bin";
    };

    systemPackages = with pkgs; [
      # usefull shell stuff
      bat
      fzf
      ripgrep

      # extracting file and dirs
      unzip
      zip
      ranger

      # standard toolset
      vim
      coreutils-full
      wget
      curl
      git

      my-handmade-hero
    ];

    shells = with pkgs; [ bash zsh ];
  };

  nix = {
    package = pkgs.nixVersions.stable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    gc.automatic = true;
  };
  nixpkgs.config.allowUnfree = true;
}
