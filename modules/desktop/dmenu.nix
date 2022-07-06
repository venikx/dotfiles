{ options, config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.desktop.dmenu;
  configDir = config.dotfiles.configDir;
in {
  options.modules.desktop.dmenu = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.venikx = {
      home.packages = with pkgs; [
        dmenu
        # Currently dmenu.xresources patch can be applied, but breaks dmenu :(
        #
        #(dmenu.overrideAttrs (oldAttrs: rec {
        #  patches = [
        #    "${configDir}/dmenu/dmenu-xresources-4.9.diff"
        #  ];
        #}))
      ];
    };
  };
}
