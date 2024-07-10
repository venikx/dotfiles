{ config, lib, pkgs, ... }: {
  options.modules.services.syncthing.enable =
    lib.mkEnableOption "Enable Syncthing";

  config = lib.mkIf config.modules.services.syncthing.enable {
    environment.systemPackages = [ pkgs.syncthing ];

    # TODO(Kevin): Move to home-manager
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
          "nas" = {
            id =
              "HX7UEBN-V4VVYXA-MAKBV6I-GKWUN4B-LA5HKEB-2WIHSFL-UYVCLTW-7LVYMQF";
          };
        };
        folders = {
          "org" = {
            path = "/home/venikx/org/gtd";
            versioning = {
              type = "simple";
              params.keep = "10";
            };
            devices = [ "kevin-iphone" "nas" ];
          };
        };
      };
    };
  };
}
