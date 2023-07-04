{ pkgs, ... }:

{
  programs.zsh.enable = true;

  environment = {

    variables = {
      XDG_CACHE_HOME  = "$HOME/.cache";
      XDG_CONFIG_HOME = "$HOME/.config";
      XDG_DATA_HOME   = "$HOME/.local/share";
      XDG_BIN_HOME    = "$HOME/.local/bin";
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
