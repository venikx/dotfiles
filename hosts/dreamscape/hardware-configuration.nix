{ config, lib, pkgs, modulesPath, ... }:

let
  unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
in {
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot = {
    initrd.availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
    initrd.kernelModules = [ ];
    extraModulePackages = [ ];
    kernelModules = [ "kvm-amd" ];
    kernelParams = [ ];
    kernelPackages = unstable.linuxPackages_latest;
  };

  modules.hardware = {
    audio.enable = true;
    bluetooth.enable = true;
    bluetooth.audio.enable = true;
    nvidia.enable = true;
    monitors.home.enable = true;
  };

  # CPU
  hardware.cpu.amd.updateMicrocode = true;

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/e26e2be0-5f75-41a0-bf3c-81df80629226";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/B612-E9FB";
      fsType = "vfat";
    };

  swapDevices = [ ];
}
