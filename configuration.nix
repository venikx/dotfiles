# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      <home-manager/nixos>
      ./hardware-configuration.nix
      ./modules/options.nix
      ./modules/xdg.nix
      ./modules/desktop/terminal/default.nix
      ./modules/desktop/terminal/st.nix
      ./modules/editors/emacs.nix
      ./modules/shell/git.nix
      ./modules/shell/zsh.nix
      ./modules/themes/default.nix
      ./modules/themes/doom/default.nix
    ];

  modules.shell.git.enable = true;
  modules.shell.zsh.enable = true;
  modules.editors.emacs.enable = true;
  modules.desktop.terminal = {
    default = "xst";
    st.enable = true;
  };
  modules.theme.active = "doom";






  nixpkgs.config.allowUnfree = true;


  # Use the systemd-boot EFI boot loader.
  # boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot";
  boot.loader.grub.enable = true;
  boot.loader.grub.devices = ["nodev"];
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.useOSProber = true;

  networking.hostName = "nixos"; # Define your hostname.
#  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
#  networking.wireless.networks = {
#    "DNA-WLAN-2G-FB10" = {
#      psk = "61547907870"; 
#    }
#  };       

  # Set your time zone.
  time.timeZone = "Europe/Helsinki";
  location = {
    latitude = 60.192059;
    longitude = 24.945831;
  };

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.wlp2s0.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  # Enable the GNOME 3 Desktop Environment.
  # services.xserver.enable = true;
  # services.xserver.displayManager.gdm.enable = true;
  # services.xserver.desktopManager.gnome3.enable = true;

  services = {
    picom.enable = true;
    redshift.enable = true;
    xserver = {
      enable = true;
      # windowManager.bspwm.enable = true;
      displayManager.gdm.enable = true;
      displayManager.gdm.wayland = false;
      desktopManager.gnome3.enable = true;
      videoDrivers = [ "amdgpu" ];
    };
  };

  systemd.user.services."dunst" = {
    enable = true;
    description = "";
    wantedBy = [ "default.target" ];
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${pkgs.dunst}/bin/dunst";
  };
  
  # Configure keymap in X11
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # sound.enable = true;
  # hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.venikx = {
    isNormalUser = true;
    initialPassword = "v3nikx";
    description = "Kevin Rangel";
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    git 
    neovim

    firefox
    pciutils
    dunst
    libnotify
    (polybar.override {
      pulseSupport = true;
      nlSupport = true;
    })
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?

}

