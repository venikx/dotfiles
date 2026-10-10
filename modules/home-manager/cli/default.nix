{ pkgs, lib, ... }:

{
  imports = [
    ./direnv.nix
    ./git.nix
    ./ssh.nix
    ./zsh.nix
  ];

  home.packages =
    with pkgs;
    [ ]
    ++ lib.optionals stdenv.hostPlatform.isLinux [
      claude-code
    ];
}
