{ config, lib, pkgs, modulesPath, nixos-apple-silicon, ... }:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    nixos-apple-silicon.nixosModules.apple-silicon-support
  ];

  boot.initrd.availableKernelModules = [ "usb_storage" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = false;

  hardware.asahi = {
    enable = true;
    withRust = true;
    #extractPeripheralFirmware = true;
    peripheralFirmwareDirectory = ./firmware;
    #useExperimentalGPUDriver = true;
    experimentalGPUInstallMode = "overlay";
    setupAsahiSound = true;
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/b1566969-a85d-4110-9f30-46e75e220566";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/BB53-08E8";
    fsType = "vfat";
  };

  swapDevices = [ ];

  nixpkgs.hostPlatform = lib.mkDefault "aarch64-linux";
  powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";
}
