{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot = {
    initrd.availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
    initrd.kernelModules = [ ];
    extraModulePackages = [ ];
    kernelModules = [ "kvm-amd" ];
    kernelParams = [ ];
    kernelPackages = pkgs.linuxPackages_latest;
    loader.grub.configurationLimit = 3;
  };

  modules.hardware = {
    audio.enable = true;
    bluetooth.enable = true;
    bluetooth.audio.enable = true;
    nvidia.enable = true;
    monitors.home.enable = true;
  };

  # Are these really needed? Seems so atm.
  hardware.enableRedistributableFirmware = true;
  # CPU
  hardware.cpu.amd.updateMicrocode = true;

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/4df3f4a4-fac6-471c-a8e1-11c03c2c093c";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/C831-07CF";
      fsType = "vfat";
    };

  swapDevices = [ ];

  home-manager.users.venikx.home.stateVersion = "22.05";
  system.stateVersion = "22.05"; # Did you read the comment?
}
