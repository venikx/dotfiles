{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.browsers;
in {
  imports =
    [
      ./brave.nix
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
    # TODO(Kevin): Check https://rycee.gitlab.io/home-manager/options.html#opt-xdg.mimeApps.defaultApplications
  };
}
