{
  config,
  lib,
  pkgs,
  ...
}:

{
  networking.networkmanager.enable = true;
  networking.hosts = {
    "172.19.20.11" = [ "proxmox.local" ];
    "172.19.20.10" = [ "truenas.local" ];
    "172.19.20.227" = [
      "homelab.local"
      "immich.homelab.local"
    ];
  };
}
