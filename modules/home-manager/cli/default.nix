{ config, pkgs, ... }:

let
  agentsLink = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/AGENTS.md";
in
{
  imports = [
    ./direnv.nix
    ./git.nix
    ./ssh.nix
    ./zsh.nix
  ];

  programs.claude-code = {
    enable = pkgs.stdenv.hostPlatform.isLinux;
    package = if pkgs.stdenv.hostPlatform.isLinux then pkgs.claude-code else null;
    mutableSettings = true;

    settings = {
      tui = "fullscreen";
      theme = "dark-daltonized";
      model = "opus";
      autoMemoryEnabled = false;
      cleanupPeriodDays = 7;
    };
  };

  home.file."AGENTS.md".source = ./AGENTS.md;
  home.file.".claude/CLAUDE.md".source = agentsLink;
  home.file.".codex/AGENTS.md".source = agentsLink;
}
