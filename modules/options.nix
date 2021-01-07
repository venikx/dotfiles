{ config, options, lib, ... }:

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
    # must already begin with pre-existing PATH. Also, can't use binDir here,
    # because it contains a nix store path.
    env.PATH = [ "$PATH" ];

    environment.extraInit =
      concatStringsSep "\n"
        (mapAttrsToList (n: v: "export ${n}=\"${v}\"") config.env);
  };
}