{
  description = "Flake to rule em all.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";

    emacs-overlay = {
      url  = "github:nix-community/emacs-overlay/master";
      #inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-22.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    darwin = {
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, darwin, emacs-overlay }:
    let
      user = "venikx";
      lib = nixpkgs.lib;
    in {
      nixosConfigurations = {
        dreamscape = lib.nixosSystem { #desktop
          system = "x86_64-linux";
          specialArgs = { inherit user home-manager emacs-overlay; };
          pkgs = import nixpkgs {
            system = "x86_64-linux";
            config.allowUnfree = true;
          };
          modules = [
            ./hosts/dreamscape

            home-manager.nixosModules.home-manager {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.extraSpecialArgs = { inherit user; };
            }
          ];
        };

        inception = lib.nixosSystem { #laptop
          system = "x86_64-linux";
          specialArgs = { inherit user home-manager emacs-overlay; };
          pkgs = import nixpkgs {
            system = "x86_64-linux";
            config.allowUnfree = true;
          };
          modules = [
            ./hosts/inception

            home-manager.nixosModules.home-manager {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.extraSpecialArgs = { inherit user; };

            }
          ];
        };
      };

      darwinConfigurations =  {
        lucid = darwin.lib.darwinSystem {
          system = "aarch64-darwin";
          specialArgs = { inherit user home-manager emacs-overlay; };
          modules = [
            ./hosts/lucid

            home-manager.darwinModules.home-manager {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.extraSpecialArgs = { inherit user; };
              # home-manager.users.${user} = import ./home.nix;
            }
          ];
        };
      };
    };
}
