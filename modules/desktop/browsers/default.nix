
{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.browsers;
in {
  imports =
    [
      ./firefox.nix
    ];

  options.modules.desktop.browsers = with types; {
    default = mkOption {
      type = nullOr str;
      default = null;
    };
  };

  config = mkIf (cfg.default != null) {
    env.BROWSER = cfg.default;
  };
}
