{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot = {
    initrd.availableKernelModules =
      [ "nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
    initrd.kernelModules = [ ];
    extraModulePackages = [ ];
    kernelModules = [ "kvm-amd" ];
    kernelParams = [ ];
    kernelPackages = pkgs.linuxPackages_latest;
    loader.grub.configurationLimit = 3;
  };

  # Are these really needed? Seems so atm.
  hardware.enableRedistributableFirmware = true;

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/4df3f4a4-fac6-471c-a8e1-11c03c2c093c";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/C831-07CF";
    fsType = "vfat";
  };

  fileSystems."/mnt/nas/documents" = {
    device = "192.168.1.182:/mnt/tank/documents";
    fsType = "nfs";
    options = [ "x-systemd.automount" "noauto" ];
  };

  fileSystems."/mnt/nas/images" = {
    device = "192.168.1.182:/mnt/tank/images";
    fsType = "nfs";
    options = [ "x-systemd.automount" "noauto" ];
  };

  swapDevices = [ ];

}
