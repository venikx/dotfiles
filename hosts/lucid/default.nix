{ user, ... }:

{
  imports = [
    ../../modules/options.nix
    ../../modules/common.nix
    ../../modules/darwin
    ../../modules/editors
  ];

  user = {
    home = "/Users/${user}";
  };

  modules = {
    editors = {
      emacs.enable = true;
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
  };

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.zsh.enable = true;  # default shell on catalina
  system.stateVersion = 4;
  home-manager.users.venikx.home.stateVersion = "22.05";
}
