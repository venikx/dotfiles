# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot = {
    initrd.availableKernelModules = [ "xhci_pci" "nvme" "ahci" "usb_storage" "usbhid" "sd_mod" ];
    initrd.kernelModules = [ ];
    kernelModules = [ "kvm-amd" ];
    kernelPackages = pkgs.linuxPackages_latest;
    extraModulePackages = [ ];
    # kernelParams = [ "nomodeset" ];
  };

  hardware.nvidia.modesetting.enable = lib.mkForce true;
  services.xserver.videoDrivers = lib.mkForce [ "nvidia" ];

  modules.hardware = {
    audio.enable = true;
    bluetooth.enable = true;
    bluetooth.audio.enable = true;
  };

  # Power Management
  environment.systemPackages = [ pkgs.acpi ];
  powerManagement.powertop.enable = true;
  # Monitor backlight control
  programs.light.enable = true;
  users.users.venikx = {
    extraGroups = [ "video" ];
  };

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/d49b4d57-2271-47d3-bb50-2c866c26093d";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/2CE1-F5B3";
      fsType = "vfat";
    };

  swapDevices = [ ];
}