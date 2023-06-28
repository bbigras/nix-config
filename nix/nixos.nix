{ self
, home-manager
, impermanence
, lanzaboote
, nix-index-database
, nixos-hardware
, nixpkgs
, stylix
, templates
, nur
, attic
, sops-nix
, agenix
, ...
}:
let
  inherit (nixpkgs) lib;

  genConfiguration = hostname: { address, hostPlatform, type, ... }:
    lib.nixosSystem {
      modules = [
        (../hosts + "/${hostname}")
        {
          nix.registry = {
            nixpkgs.flake = nixpkgs;
            p.flake = nixpkgs;
            templates.flake = templates;
          };
          nixpkgs.pkgs = self.pkgs.${hostPlatform};
        }
        agenix.nixosModules.default
      ];
      specialArgs = {
        hostAddress = address;
        hostType = type;
        inherit attic home-manager impermanence lanzaboote nix-index-database nixos-hardware nur stylix sops-nix;
      };
    };
in
lib.mapAttrs genConfiguration (self.hosts.nixos or { })
