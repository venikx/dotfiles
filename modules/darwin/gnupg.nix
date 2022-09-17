

{ config, lib, pkgs, options, ... }:

with lib;
{
  environment.variables.GNUPGHOME = "${config.environment.variables.XDG_CONFIG_HOME}/gnupg";
  modules.shell.zsh.rcInit = ''
      export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
      gpgconf --launch gpg-agent
    '';

  homebrew.brews = [ "pinentry-mac" "gnupg" "hopenpgp-tools"];

  home-manager.users.venikx = {
    home.packages = with pkgs; [
      yubikey-manager
      yubikey-personalization
    ];

    xdg.configFile."gnupg/gpg-agent.conf" = {
      text = ''
          enable-ssh-support
          default-cache-ttl 300
          max-cache-ttl 3600
          pinentry-program /opt/homebrew/bin/pinentry-mac
        '';
    };
  };
}
