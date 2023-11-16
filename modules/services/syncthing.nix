{ config, lib, pkgs, ... }: {
  options.modules.services.syncthing.enable =
    lib.mkEnableOption "Enable Syncthing";

  config = lib.mkIf config.modules.services.syncthing.enable {
    environment.systemPackages = [ pkgs.syncthing ];

    services.syncthing = {
      enable = true;
      user = "venikx";
      dataDir = "/home/venikx/docs/syncthing";
      configDir = "/home/venikx/docs/syncthing/.config";
      overrideDevices = true;
      overrideFolders = true;
    };
  };
}
