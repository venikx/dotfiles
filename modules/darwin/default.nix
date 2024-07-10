{ ... }: {
  imports = [ ./gnupg.nix ./homebrew.nix ../common.nix ];

  services.nix-daemon.enable = true;
}
