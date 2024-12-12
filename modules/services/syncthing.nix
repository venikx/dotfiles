{ config, lib, pkgs, ... }: {
  options.modules.services.syncthing.enable =
    lib.mkEnableOption "Enable Syncthing";

  config = lib.mkIf config.modules.services.syncthing.enable {
    environment.systemPackages = [ pkgs.syncthing ];

    services.syncthing = {
      enable = true;
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
              "CEA6NZ3-DEAXTTD-X7VFJY6-K37376N-RJYWZG5-AT7TBIH-YQVDG6A-C3FUWA7";
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
