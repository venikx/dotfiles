{
  description = "Flake to rule em all.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    emacs-overlay = {
      url  = "github:nix-community/emacs-overlay/master";
      #inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    darwin = {
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, nixos-hardware, home-manager, darwin, emacs-overlay }:
    let
      lib = nixpkgs.lib;
    in {
      nixosConfigurations = {
        earth = lib.nixosSystem rec { #desktop
          system = "x86_64-linux";
          specialArgs = { inherit home-manager emacs-overlay; };
          modules = [
            ./hosts/${system}/earth
            ./modules/nixos
            nixos-hardware.nixosModules.common-pc
            nixos-hardware.nixosModules.common-pc-ssd
            nixos-hardware.nixosModules.common-cpu-amd
            nixos-hardware.nixosModules.common-gpu-nvidia-nonprime

            home-manager.nixosModules.home-manager {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.venikx = lib.mkMerge [
                (import ./modules/home-manager)
                (import ./modules/home-manager/nixos)
                (import ./hosts/${system}/earth/venikx.nix)
              ];
            }
          ];
        };

        fire = lib.nixosSystem rec { #laptop
          system = "x86_64-linux";
          specialArgs = { inherit home-manager emacs-overlay; };
          modules = [
            ./hosts/${system}/fire
            ./modules/nixos
            nixos-hardware.nixosModules.asus-zephyrus-ga401

            home-manager.nixosModules.home-manager {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.venikx = lib.mkMerge [
                (import ./modules/home-manager)
                (import ./modules/home-manager/nixos)
                (import ./hosts/${system}/fire/venikx.nix)
              ];
            }
          ];
        };

        air = lib.nixosSystem rec { # asahi macbook
          system = "aarch64-linux";
          specialArgs = { inherit home-manager emacs-overlay; };
          modules = [
            ./hosts/${system}/air
            ./modules/nixos

            home-manager.nixosModules.home-manager {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.venikx = lib.mkMerge [
                (import ./modules/home-manager)
                (import ./modules/home-manager/nixos)
                (import ./hosts/${system}/air/venikx.nix)
              ];
            }
          ];
        };
      };

      darwinConfigurations =  {
        lucid = darwin.lib.darwinSystem rec {
          system = "aarch64-darwin";
          specialArgs = { inherit home-manager emacs-overlay; };
          modules = [
            ./hosts/${system}/lucid
            ./modules/darwin

            home-manager.darwinModules.home-manager {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.venikx = lib.mkMerge [
                (import ./modules/home-manager)
                (import ./hosts/${system}/lucid/venikx.nix)
              ];
            }
          ];
        };
      };
    };
}
