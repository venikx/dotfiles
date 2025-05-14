{
  description = "Flake to rule em all.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    nix-colors.url = "github:misterio77/nix-colors";

    nixos-apple-silicon = {
      url = "github:tpwrules/nixos-apple-silicon";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    darwin = {
      url = "github:nix-darwin/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, nixos-hardware, nixos-apple-silicon, home-manager
    , darwin, emacs-overlay, nix-colors, disko }:
    let lib = nixpkgs.lib;
    in {
      nixosConfigurations = {
        earth = lib.nixosSystem rec { # desktop
          system = "x86_64-linux";
          specialArgs = { inherit home-manager emacs-overlay; };
          modules = [
            ./hosts/${system}/earth
            ./modules/nixos
            nixos-hardware.nixosModules.common-pc
            nixos-hardware.nixosModules.common-pc-ssd
            nixos-hardware.nixosModules.common-cpu-amd
            nixos-hardware.nixosModules.common-gpu-nvidia-nonprime

            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.venikx = lib.mkMerge [
                (import ./modules/home-manager/nixos.nix)
                (import ./hosts/${system}/earth/venikx.nix)
                nix-colors.homeManagerModules.default
              ];
              home-manager.extraSpecialArgs = { inherit nix-colors; };
            }
          ];
        };

        limber-lt-kdb = lib.nixosSystem rec { # laptop
          system = "x86_64-linux";
          specialArgs = { inherit home-manager emacs-overlay; };
          modules = [
            disko.nixosModules.disko
            nixos-hardware.nixosModules.framework-13-7040-amd
            ./hosts/${system}/limber-lt-kdb
            ./modules/nixos

            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.venikx = lib.mkMerge [
                (import ./modules/home-manager/nixos.nix)
                (import ./hosts/${system}/limber-lt-kdb/venikx.nix)
                nix-colors.homeManagerModules.default
              ];
              home-manager.extraSpecialArgs = { inherit nix-colors; };
            }
          ];
        };

        air = lib.nixosSystem rec { # asahi macbook
          system = "aarch64-linux";
          specialArgs = {
            inherit home-manager emacs-overlay nixos-apple-silicon;
          };
          modules = [
            ./hosts/${system}/air
            ./modules/nixos

            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.venikx = lib.mkMerge [
                (import ./modules/home-manager/nixos.nix)
                (import ./hosts/${system}/air/venikx.nix)
                nix-colors.homeManagerModules.default
              ];
              home-manager.extraSpecialArgs = { inherit nix-colors; };
            }
          ];
        };
      };

      darwinConfigurations = {
        "MacBook-Y026V" = darwin.lib.darwinSystem rec {
          system = "aarch64-darwin";
          specialArgs = { inherit home-manager emacs-overlay; };
          modules = [
            ./hosts/${system}/MacBook-Y026V
            ./modules/darwin

            home-manager.darwinModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.kdebaerdemaeker = lib.mkMerge [
                (import ./modules/home-manager/darwin.nix)
                (import ./hosts/${system}/MacBook-Y026V/venikx.nix)
                nix-colors.homeManagerModules.default
              ];
              home-manager.extraSpecialArgs = { inherit nix-colors; };
            }
          ];
        };
      };
    };
}
