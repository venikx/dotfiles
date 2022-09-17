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
        # TODO(Kevin): Set env variable for nixos to store dotfiles under
        # .config/dotfiles
        default = (findFirst pathExists (toString ../.) [
          "/etc/nixos" # nixos
          "${config.user.home}/.config/dotfiles" # darwin, macbook
        ]);
      };

      configDir = mkOption {
        type = path;
        default = "${config.dotfiles.dir}/config";
      };
    };

    # deprecated, prefer to set environment.variables or via home.
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
      name = "venikx";
      description = "Kevin Rangel";
    };

    env.PATH = ["${config.dotfiles.dir}/bin" "$PATH" ];
    users.users.${config.user.name} = mkAliasDefinitions options.user;

    environment.extraInit =
      concatStringsSep "\n"
        (mapAttrsToList (n: v: "export ${n}=\"${v}\"") config.env);
  };
}
