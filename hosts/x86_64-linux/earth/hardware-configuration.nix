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
  };

  # Are these really needed? Seems so atm.
  hardware.enableRedistributableFirmware = true;

  swapDevices = [ ];
}
