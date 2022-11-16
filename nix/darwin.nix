{ self
, darwin
, home-manager
, nixpkgs
, templates
, ...
}:
let
  inherit (nixpkgs) lib;
  hosts = (import ./hosts.nix).nix-darwin.all;

  nixRegistry = {
    nix.registry = {
      nixpkgs.flake = nixpkgs;
      p.flake = nixpkgs;
      pkgs.flake = nixpkgs;
      templates.flake = templates;
    };
  };

  genConfiguration = hostname: { hostPlatform, ... }:
    darwin.lib.darwinSystem {
      system = hostPlatform;
      pkgs = self.pkgs.${hostPlatform};
      modules = [
        (../hosts + "/${hostname}")
        nixRegistry
        home-manager.darwinModules.home-manager
      ];
    };
in
lib.mapAttrs genConfiguration hosts
