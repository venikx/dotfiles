{ user, ... }:

{
  imports = [
    ../../modules/options.nix
    ../../modules/common.nix
    ../../modules/darwin
  ];

  user = {
    home = "/Users/${user}";
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

  services.nix-daemon.enable = true;
  programs.zsh.enable = true;  # default shell on catalina
  system.stateVersion = 4;
  home-manager.users.venikx.home.stateVersion = "22.05";
}
