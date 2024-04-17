{ withSystem, inputs, ... }:

let
  inherit (inputs.nixpkgs) lib;

  genConfiguration = hostname: { address, hostPlatform, type, ... }:
    withSystem hostPlatform ({ pkgs, ... }:
      lib.nixosSystem {
        modules = [
          (../hosts + "/${hostname}")
          {
            nix.registry = {
              nixpkgs.flake = inputs.nixpkgs;
              p.flake = inputs.nixpkgs;
            };
            nixpkgs.pkgs = pkgs;
          }
        ];
        specialArgs = {
          hostAddress = address;
          hostType = type;
          inherit (inputs)
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
      });
in
lib.mapAttrs
  genConfiguration
  (lib.filterAttrs (_: host: host.type == "nixos") inputs.self.hosts)
