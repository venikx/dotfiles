{ config, lib, pkgs, options, ... }:

with lib;
{
  homebrew.brews = [ "pinentry-mac"];
  programs.gnupg.agent = {
    enable = true;
  };

  home-manager.users.venikx = {
    home = {
      packages = with pkgs; [
        yubikey-manager
        yubikey-personalization
      ];
    };

    programs.zsh.initExtra = ''
        gpg-connect-agent /bye
        export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
        gpgconf --launch gpg-agent
    '';

    programs.gpg = {
      enable = true;
      homedir = "${config.users.users.venikx.home}/.config/gnupg";
      scdaemonSettings = {
        disable-ccid = true;
      };
    };

    xdg.configFile."gnupg/gpg-agent.conf" = {
      text = ''
          homedir ${config.environment.variables.XDG_CONFIG_HOME}/gnupg
          enable-ssh-support
          default-cache-ttl 300
          max-cache-ttl 3600
          pinentry-program /opt/homebrew/bin/pinentry-mac
        '';
    };
  };
}
