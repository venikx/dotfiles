{ self, lib, pkgs, ... }:

{
  users.users.kdebaerdemaeker = {
    name = "kdebaerdemaeker";
    description = "Kevin De Baerdemaeker";
    home = "/Users/kdebaerdemaeker";
    shell = pkgs.zsh;
  };

  # Set primary user for homebrew and other user-specific options
  system.primaryUser = "kdebaerdemaeker";

  environment.systemPath = [ 
    # "/opt/homebrew/bin" 
  ];

  system.stateVersion = 5;
  nixpkgs.hostPlatform = lib.mkDefault "aarch64-darwin";

  nix.enable = false;
  nix.gc.automatic = lib.mkForce false;
}