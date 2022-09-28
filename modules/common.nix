{ pkgs, ... }:

{
  imports = [
    ./home-manager
  ];

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    enableBashCompletion = true;
  };

  environment = {
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

    shells = with pkgs; [ bash zsh fish ];
  };

  nix = {
    package = pkgs.nixVersions.stable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    gc.automatic = true;
  };
  nixpkgs.config.allowUnfree = true;

  # can I remove this?
  nixpkgs.config.permittedInsecurePackages = [
    "openssl-1.0.2u"
  ];
}
