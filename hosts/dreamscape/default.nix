{ ... }:

{
  imports = [
    ../personal.nix
    ./hardware-configuration.nix
  ];

  networking = {
    hostName = "dreamscape";
    networkmanager.enable = true;
    useDHCP = false;
    interfaces.enp34s0.useDHCP = true;
  };

  modules = {
    desktop = {
      bspwm.enable = true;
      dmenu.enable = true;
      communication.enable = true;
      browsers = {
        default = "firefox";
        firefox.enable = true;
      };
      media = {
        spotify.enable = true;
        streaming.enable = true;
        music-production.enable = true;
        readers = {
          pdf.enable = true;
          ebook.enable = true;
        };
      };
      terminal = {
        default = "xst";
        st.enable = true;
      };
      gaming = {
        steam.enable = true;
        emulators = {
          psx.enable = true;
          ds.enable = true;
          gba.enable = true;
          gb.enable = true;
          snes.enable = true;
        };
      };
    };
    editors = {
      default = "nvim";
      emacs.enable = true;
      vim.enable = true;
    };
    dev = {
      shell.enable = true;
      node.enable = true;
      node.binaries.enable = true;
      cc.enable = true;
      rust.enable = true;
      rust.binaries.enable = true;
    };
    shell = {
      git.enable = true;
      zsh.enable = true;
      direnv.enable = true;
    };
    services = {
      docker.enable = true;
    };
    theme.active = "doom";
  };

# boot.loader.systemd-boot.enable = true;
# boot.loader.efi.canTouchEfiVariables = true;

  boot = {
    loader.efi.canTouchEfiVariables = true;
    loader.efi.efiSysMountPoint = "/boot";
    loader.grub.enable = true;
    loader.grub.devices = ["nodev"];
    loader.grub.efiSupport = true;
    loader.grub.useOSProber = true;
  };
}

