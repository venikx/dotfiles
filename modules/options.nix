{ config, options, lib, pkgs, ... }:

with builtins;
with lib;

{
  options = with types; {
    env = mkOption {
      type = attrsOf (oneOf [ str path (listOf (either str path)) ]);
      apply = mapAttrs
        (n: v: if isList v
               then concatMapStringsSep ":" (x: toString x) v
               else (toString v));
      default = {};
      description = "TODO";
    };
  };

  config = {
    # Define a user account. Don't forget to set a password with ‘passwd’.
    users.users.venikx = mkMerge [
      {
        description = "Kevin Rangel";
      }
      (mkIf pkgs.stdenv.isLinux {
        isNormalUser = true;
        extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
        initialPassword = "v3nikx";
      })

    ];

    home-manager.users.venikx = {
      home.file.".local/bin" = {
        source = "/etc/nixos/bin";
        recursive = true;
      };
    };
    # must already begin with pre-existing PATH. Also, can't use binDir here,
    # because it contains a nix store path.
    env.PATH = [ "$XDG_BIN_HOME" "$PATH" ];

    environment.extraInit =
      concatStringsSep "\n"
        (mapAttrsToList (n: v: "export ${n}=\"${v}\"") config.env);
  };
}
