{ ... }:
{
  imports =
    [
      ./gnupg.nix
      ./homebrew.nix
      ../common.nix
      ../options.nix
    ];

  services.nix-daemon.enable = true;
}
