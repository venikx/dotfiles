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
      settings = {
        devices = {
          "kevin-iphone" = {
            id =
              "ELIRVGI-W7IUR4D-HRK25JP-HML6LPG-QEH76SY-BTAL4Q6-YO66RTD-SOP37AL";
          };
        };
        folders = {
          "org" = { # Name of folder in Syncthing, also the folder ID
            path = "/home/venikx/org/gtd"; # Which folder to add to Syncthing
            devices =
              [ "kevin-iphone" ]; # Which devices to share the folder with
          };
        };
      };
    };

  };
}
