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
  hardware.video.hidpi.enable = lib.mkDefault true;
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
  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?
}
