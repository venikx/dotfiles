{ ... }:

{
  imports = [
    ../../modules
    ../nix.nix
    ../linux.nix
    ./hardware-configuration.nix
  ];

  networking = {
    hostName = "inception";
    networkmanager.enable = true;
    useDHCP = false;
    interfaces.wlp2s0.useDHCP = true;
  };

  modules = {
    desktop = {
      bspwm.enable = true;
      dmenu.enable = true;
      browsers = {
        default = "brave";
        firefox.enable = true;
        brave.enable = true;
      };
      media = {
        spotify.enable = true;
        readers = {
          pdf.enable = true;
          ebook.enable = true;
        };
        backup.enable = true;
        music-production.enable = true;
      };
      terminal = {
        default = "xst";
        st.enable = true;
      };
      gaming = {};
    };
    editors = {
      emacs.enable = true;
      vim.enable = true;
    };
    dev = {
      cc.enable = true;
      node.enable = true;
      node.binaries.enable = false;
      rust.enable = true;
      rust.binaries.enable = false;
      shell.enable = true;
    };
    shell = {
      git.enable = true;
      zsh.enable = true;
      direnv.enable = true;
      gnupg.enable = true;
    };
    services = {
      docker.enable = true;
    };
    theme.active = "doom";
  };

  boot = {
    loader.efi.canTouchEfiVariables = true;
    loader.efi.efiSysMountPoint = "/boot";
    loader.grub.enable = true;
    loader.grub.devices = ["nodev"];
    loader.grub.efiSupport = true;
    loader.grub.useOSProber = true;
  };
}
