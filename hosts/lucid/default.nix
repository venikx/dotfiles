{ ... }:

{
  imports = [
    ../nix.nix
    ../../modules/options.nix
    ../../modules/editors
    ../../modules/shell/git.nix
    ../../modules/shell/gnupg.nix
    ../../modules/shell/direnv.nix
  ];

  user = {
    home = "/Users/venikx";
  };

  modules = {
    editors = {
      emacs.enable = true;
      vim.enable = true;
    };
    shell = {
      git.enable = true;
      #zsh.enable = true;
      direnv.enable = true;
      gnupg.enable = true;
    };
  };

  environment.systemPath = [ "/opt/homebrew/bin" ];
  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true;
      cleanup = "uninstall";
      upgrade = true;
    };
    masApps = {
      Xcode = 497799835;
      "1Password 7 - Password Manager" = 1333542190;
    };
  };

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.zsh.enable = true;  # default shell on catalina
  system.stateVersion = 4;
  home-manager.users.venikx.home.stateVersion = "22.05";
}
