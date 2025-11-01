{ withSystem, inputs, ... }:

let
  inherit (inputs.nixpkgs) lib;

  genConfiguration =
    hostname:
    {
      address,
      hostPlatform,
      type,
      type2,
      ...
    }:
    withSystem hostPlatform (
      { pkgs, ... }:
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
        ]
        ++ lib.optionals (type2 == "desktop") [
          inputs.srvos.nixosModules.desktop
        ]
        ++ lib.optionals (type2 == "server") [
          inputs.srvos.nixosModules.server
        ];
        specialArgs = {
          hostAddress = address;
          hostType = type;
          system = hostPlatform;
          inherit (inputs)
            catppuccin
            disko
            home-manager
            impermanence
            lanzaboote
            nix-index-database
            nixos-avf
            nixos-facter-modules
            nixos-hardware
            nixpkgs-veilid
            nur
            sops-nix
            minimal-emacs-d
            ;
        };
      }
    );
in
lib.mapAttrs genConfiguration (lib.filterAttrs (_: host: host.type == "nixos") inputs.self.hosts)
