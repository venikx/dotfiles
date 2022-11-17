{ pkgs, ... }:
{
  imports =
    [
      ./gnupg.nix
    ];

  hardware.keyboard.zsa.enable = true;
  environment.systemPackages = with pkgs; [
    wally-cli
  ];
}
