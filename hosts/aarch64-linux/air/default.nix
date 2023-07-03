{ config, pkgs, ... }:

{
  imports =
    [
     ../../../modules
     ../../../modules/common.nix
     ../../../modules/nixos
     ../../linux.nix
     ./hardware-configuration.nix
     ./apple-silicon-support
    ];

  home-manager.users.venikx.imports = [
    ../../../modules/home-manager/nixos
  ];

  networking = {
	  hostName = "air";
	  networkmanager.enable = true;
  };

  modules = {
    desktop = {
      bspwm.enable = true;
      dmenu.enable = true;
      terminal = {
        default = "xst";
        st.enable = true;
      };
    };
    editors = {
      emacs.enable = true;
    };
    theme.active = "doom";
  };

  boot = {
	  loader.systemd-boot.enable = true;
	  loader.efi.canTouchEfiVariables = false;
  };

  hardware.asahi.peripheralFirmwareDirectory = ./firmware;

  home-manager.users.venikx.home.stateVersion = "23.05";
  system.stateVersion = "23.11"; # Did you read the comment?

}

