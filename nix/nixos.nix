{ self
, home-manager
, impermanence
, nix-index-database
, nixos-hardware
, nixpkgs
, ragenix
, templates
, nur
, attic
, sops-nix
, agenix
, agenix-rekey
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
        agenix-rekey.nixosModules.default
      ];
      specialArgs = {
        hostAddress = address;
        hostType = type;
        inherit home-manager impermanence nix-index-database nixos-hardware ragenix nur attic sops-nix;
      };
    };
in
lib.mapAttrs genConfiguration (self.hosts.nixos or { })
