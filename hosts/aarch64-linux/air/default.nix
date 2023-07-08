{ config, pkgs, lib, ... }:

{
  imports =
    [
     ./hardware-configuration.nix
     ./apple-silicon-support
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

  time.timeZone = "Europe/Brussels";

  networking = {
    hostName = "air";
    useDHCP = lib.mkDefault true;
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
    dev = {
      cc.enable = true;
      clojure.enable = true;
      clojure.binaries.enable = false;
      node.enable = true;
      node.binaries.enable = false;
      rust.enable = true;
      rust.binaries.enable = false;
      shell.enable = true;
    };
    services = {
      docker.enable = true;
      tailscale.enable = true;
    };
    theme.active = "doom";
  };

  system.stateVersion = "23.11";
}
