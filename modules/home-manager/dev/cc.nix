{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkEnableOption mkWrapper;
  cfg = config.modules.dev.cc;
in {
  options.modules.dev.cc = {
    enable = mkEnableOption "Enable C/C++ development crap";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      clang
      clang-tools
      (mkWrapper gdb ''
        wrapProgram "$out/bin/gdb" --add-flags '-q -x "${config.xdg.configHome}/gdb/init"'
      '')
      man-pages # https://www.kernel.org/doc/man-pages/
    ];
  };
}
