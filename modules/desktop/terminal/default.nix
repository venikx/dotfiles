{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.terminal;
in {
  imports = [./st.nix];

  options.modules.desktop.terminal = with types; {
    default = mkOption {
      type = str;
      default = "xterm";
    };
  };

  config = {
    services.xserver.desktopManager.xterm.enable = mkDefault (cfg.default == "xterm");
    env.TERMINAL = cfg.default;
  };
}
