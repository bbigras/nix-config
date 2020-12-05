{
  description = "Personal NixOS configs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable-small";
    home-manager = {
      url = "github:rycee/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nur.url = "github:nix-community/NUR";
    deploy-rs = {
      url = "github:serokell/deploy-rs";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    impermanence.url = "github:nix-community/impermanence";
    nixos-hardware.url = "github:NixOS/nixos-hardware";

    #   mjlbach/emacs-pgtk-nativecomp-overlay
    #   lastquestion/explain-pause-mode
    #   cachix/pre-commit-hooks.nix
    #   mnacamura/nixpkgs-cdda-mods
    #   mozilla/nixpkgs-mozilla

    nixpkgs-cdda-mods = {
      url = "github:mnacamura/nixpkgs-cdda-mods";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, impermanence, nixos-hardware, deploy-rs, home-manager, nur, nixpkgs-cdda-mods, emacs-overlay, ... }@inputs:
    let
      inherit (nixpkgs.lib) nixosSystem filterAttrs const recursiveUpdate optionalAttrs;
      inherit (builtins) readDir mapAttrs;
      system = "x86_64-linux";
      systems = mapAttrs (path: _: import (./systems + "/${path}"))
        (filterAttrs (_: t: t == "directory") (readDir ./systems));
      mkSystem = config:
        nixosSystem {
          inherit system;
          modules =
            [
              impermanence.nixosModules.impermanence
              home-manager.nixosModules.home-manager
              ({
                nixpkgs.overlays = [
                  nur.overlay
                  (import "${nixpkgs-cdda-mods}")
                  emacs-overlay.overlay
                ];
              })
              ./lib/home.nix
              config

              # ./common.nix
            ];
          specialArgs = {
            inputs = inputs;
          } // optionalAttrs (builtins.pathExists ./lib/modules) {
            localModulesPath = builtins.toString ./lib/modules;
          };
        };

      deployChecks =
        mapAttrs (_: lib: lib.deployChecks self.deploy) deploy-rs.lib;

      checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;
    in
    {
      nixosConfigurations = mapAttrs (const mkSystem) systems;

      deploy.magicRollback = true;
      deploy.autoRollback = true;

      deploy.nodes = mapAttrs
        (_: nixosConfig: {
          hostname = nixosConfig.config.networking.hostName;
          profiles.system.user = "root";
          profiles.system.path = deploy-rs.lib.${system}.activate.nixos nixosConfig;
        })
        self.nixosConfigurations;

      devShell = builtins.mapAttrs
        (system: deploy:
          let pkgs = nixpkgs.legacyPackages.${system};
          in
          pkgs.mkShell {
            buildInputs =
              [
                deploy
                # (terraformFor pkgs)
                pkgs.nixUnstable
                pkgs.nix-build-uncached
              ];
          })
        deploy-rs.defaultPackage;

      checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;
    };
}
