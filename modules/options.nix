{ config, options, lib, ... }:

with builtins;
with lib;

{
  options = with types; {
    user = mkOption {
      type = attrs;
      default = {};
    };

    dotfiles = {
      dir = mkOption {
        type = path;
        default = (findFirst pathExists (toString ../.) [
          "${config.user.home}/.config.dotfiles"
          "/etc/nixos"
        ]);
      };

      binDir = mkOption {
        type = path;
        default = "${config.dotfiles.dir}/bin";
      };

      configDir = mkOption {
        type = path;
        default = "${config.dotfiles.dir}/config";
      };
    };

    env = mkOption {
      type = attrsOf (oneOf [ str path (listOf (either str path)) ]);
      apply = mapAttrs
        (n: v: if isList v
               then concatMapStringsSep ":" (x: toString x) v
               else (toString v));
      default = {};
      description = "The variables added here are exported after init.";
    };
  };

  config = {
    user = {
      isNormalUser = true;
      name = "venikx";
      description = "Kevin Rangel";
      home = "/home/venikx";
      extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
      # Define a user account. Don't forget to set a password with ‘passwd’.
      initialPassword = "v3nikx";
    };

    environment.variables.DOTFILES = config.dotfiles.dir;
    environment.variables.DOTFILES_BIN = config.dotfiles.binDir;

    users.users.${config.user.name} = mkAliasDefinitions options.user;

    # must already begin with pre-existing PATH. Also, can't use binDir here,
    # because it contains a nix store path.
    env.PATH = [ "$DOTFILES_BIN" "$XDG_BIN_HOME" "$PATH" ];

    environment.extraInit =
      concatStringsSep "\n"
        (mapAttrsToList (n: v: "export ${n}=\"${v}\"") config.env);
  };
}
