{ config, lib, pkgs, ... }:

{
  config = {
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
              "3DVU6L7-MF64HJB-5CNUSJ7-77WNJSK-GNCVXQA-BECVQCZ-ZZZCFQO-DV37GAG";
          };
        };
        folders = {
          "org" = {
            path = "${config.home.homeDirectory}/org/gtd";
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
