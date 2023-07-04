{ ... }:

{
  imports = [
    ./hardware-configuration.nix
  ];

  programs.zsh.enable = true;
  users.users.venikx = {
    name = "venikx";
    description = "Kevin De Baerdemaeker";
    home = "/home/venikx";
    shell = pkgs.zsh;
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    initialPassword = "v3nikx";
  };

  networking = {
    hostName = "inception";
    useDHCP = false;
    interfaces.wlp2s0.useDHCP = true;
  };

  modules = {
    desktop = {
      bspwm.enable = true;
      dmenu.enable = true;
      media = {
        spotify.enable = true;
        readers = {
          pdf.enable = true;
          ebook.enable = true;
        };
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