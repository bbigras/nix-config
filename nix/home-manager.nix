{ withSystem, inputs, ... }:

let
  inherit (inputs) self home-manager nixpkgs;
  inherit (nixpkgs) lib;

  genModules = hostName: { homeDirectory, ... }:
    { config, pkgs, ... }: {
      imports = [ (../hosts + "/${hostName}") ];
      nix.registry = {
        nixpkgs.flake = nixpkgs;
        p.flake = nixpkgs;
      };

      home = {
        inherit homeDirectory;
        sessionVariables.NIX_PATH = lib.concatStringsSep ":" [
          "nixpkgs=${config.xdg.dataHome}/nixpkgs"
        ];
      };

      xdg = {
        dataFile.nixpkgs.source = nixpkgs;
        configFile."nix/nix.conf".text = ''
          flake-registry = ${config.xdg.configHome}/nix/registry.json
        '';
      };
    };

  genConfiguration = hostName: { hostPlatform, type, ... }@attrs:
    withSystem hostPlatform ({ pkgs, ... }:
      home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [ (genModules hostName attrs) ];
        extraSpecialArgs = {
          hostType = type;
          inherit (inputs)
            catppuccin
            impermanence
            nix-index-database;
        };
      });
in
lib.mapAttrs
  genConfiguration
  (lib.filterAttrs (_: host: host.type == "home-manager") self.hosts)
