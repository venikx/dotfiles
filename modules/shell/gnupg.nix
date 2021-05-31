{ config, lib, pkgs, options, ... }:

with lib;
let
  cfg = config.modules.shell.gnupg;
in {
  options.modules.shell.gnupg = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    environment.variables.GNUPGHOME = "$XDG_CONFIG_HOME/gnupg";
    environment.shellInit = ''
      gpg-connect-agent /bye
      export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
    '';

    services.pcscd.enable = true;
    services.udev.packages = [ pkgs.yubikey-personalization ];

    programs.ssh.startAgent = false;
    programs.gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };

    home-manager.users.venikx = {
      home.packages = with pkgs; [
        yubikey-manager-qt
        yubikey-personalization
      ];
      
      xdg.configFile."gnupg/gpg-agent.conf" = {
        text = ''
          enable-ssh-support
          default-cache-ttl 300
          max-cache-ttl 3600
          pinentry-program ${pkgs.pinentry.qt}/bin/pinentry
        '';
      };
    };
  };
}

