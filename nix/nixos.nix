{ self
, base16-schemes
, home-manager
, impermanence
, lanzaboote
, nix-index-database
, nixos-hardware
, nixpkgs
, stylix
, templates
, sops-nix
, agenix
, rycee-nur-expressions
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
        inherit
          base16-schemes
          home-manager
          impermanence
          lanzaboote
          nix-index-database
          nixos-hardware
          rycee-nur-expressions
          sops-nix
          stylix;
      };
    };
in
lib.mapAttrs genConfiguration (self.hosts.nixos or { })
