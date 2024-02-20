{ self
, base16-schemes
, home-manager
, impermanence
, lanzaboote
, nix-index-database
, nixos-hardware
, nixpkgs
, stylix
, nur
, sops-nix
, agenix
, srvos
, ...
}:
let
  inherit (nixpkgs) lib;

  genConfiguration = hostname: { address, hostPlatform, type, type2, ... }:
    lib.nixosSystem {
      modules = [
        (../hosts + "/${hostname}")
        {
          nix.registry = {
            nixpkgs.flake = nixpkgs;
            p.flake = nixpkgs;
          };
          nixpkgs.pkgs = self.pkgs.${hostPlatform};
        }
        agenix.nixosModules.default
      ] ++ lib.optionals (type2 == "desktop") [
        srvos.nixosModules.desktop
      ] ++ lib.optionals (type2 == "server") [
        srvos.nixosModules.server
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
          nur
          sops-nix
          stylix;
      };
    };
in
lib.mapAttrs
  genConfiguration
  (lib.filterAttrs (_: host: host.type == "nixos") self.hosts)
