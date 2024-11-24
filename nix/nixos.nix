{ withSystem, inputs, ... }:

let
  inherit (inputs.nixpkgs) lib;

  genConfiguration = hostname: { address, hostPlatform, type, type2, ... }:
    withSystem hostPlatform ({ pkgs, ... }:
      lib.nixosSystem {
        modules = [
          inputs.nixos-cosmic.nixosModules.default
          (../hosts + "/${hostname}")
          {
            nix.registry = {
              nixpkgs.flake = inputs.nixpkgs;
              p.flake = inputs.nixpkgs;
            };
            nixpkgs.pkgs = pkgs;
          }
        ] ++ lib.optionals (type2 == "desktop") [
          inputs.srvos.nixosModules.desktop
        ] ++ lib.optionals (type2 == "server") [
          inputs.srvos.nixosModules.server
        ];
        specialArgs = {
          hostAddress = address;
          hostType = type;
          inherit (inputs)
            catppuccin
            home-manager
            impermanence
            lanzaboote
            nix-index-database
            nixos-hardware
            nur
            sops-nix;
        };
      });
in
lib.mapAttrs
  genConfiguration
  (lib.filterAttrs (_: host: host.type == "nixos") inputs.self.hosts)
