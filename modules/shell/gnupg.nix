{ config, lib, pkgs, options, ... }:

#TODO(Kevin): It might be better to seperate this file completely between Linux and Mac
# The home-manager only is capable of configuring systemd services. Therefore the gpg-agent
# can't be configured using home-manager. Not sure if we should separate, or just handle it
# for both systems
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
  config = mkIf (cfg.enable) (mkMerge [
    (mkIf pkgs.stdenv.isLinux {
      services.pcscd.enable = true;
      services.udev.packages = [ pkgs.yubikey-personalization ];
    })
    (mkIf pkgs.stdenv.isDarwin {
      #homebrew.brews = [ "pinentry-mac"];
    })
    {
    programs.gnupg.agent = {
      enable = true;
    };

    home-manager.users.venikx = {
      xsession.windowManager.bspwm.rules = {
        "Pinentry" = {
          state = "floating";
          center = true;
        };
      };

      home = {
        #TODO(Kevin): Extract out as these are yubikey specific and not pure gpg
        packages = with pkgs; [
          yubikey-manager
          yubikey-personalization
        ];
        sessionVariables = {
          GNUPGHOME = "${config.environment.variables.XDG_CONFIG_HOME}/gnupg";
        };
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
          pinentry-program ${if pkgs.stdenv.isDarwin then /opt/homebrew/bin/pinentry-mac else "${pkgs.pinentry.qt}/bin/pinentry" }
        '';
      };
    };
    }]);
}

