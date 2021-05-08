{ self
, deploy-rs
, home-manager
, impermanence
, nixpkgs
, sops-nix
, nur
, nixpkgs-cdda-mods
, nix-matrix-pinecone
, emacs-overlay
, nix-direnv
, ...
}@inputs:
let
  inherit (nixpkgs.lib) pathExists optionalAttrs;
  inherit (builtins) attrNames mapAttrs readDir;

  pkg_pinecone = import nix-matrix-pinecone;
  nix-direnv2 = import nix-direnv { };

  overlays = [
    nur.overlay
    (import "${nixpkgs-cdda-mods}")
    emacs-overlay.overlay
    (import ../overlays/nix-zsh-completions.nix)

    (self: super: {
      nix-matrix-pinecone = pkg_pinecone.main;
    })

    (self: super: {
      nix-direnv = nix-direnv2;
    })
  ];

  mkHost = name: system:
    nixpkgs.lib.nixosSystem {
      inherit system;
      modules = [
        ({ nixpkgs = { inherit overlays; }; })

        impermanence.nixosModules.impermanence
        home-manager.nixosModules.home-manager
        sops-nix.nixosModules.sops

        ../lib/home.nix

        (../hosts + "/${name}")
      ];
      specialArgs.inputs = inputs;
    };

  mkPath = name: system: deploy-rs.lib.${system}.activate.nixos (mkHost name system);
in
{
  deploy = {
    autoRollback = true;
    magicRollback = true;
    user = "root";
    nodes = {
      desktop = {
        hostname = "desktop";
        profiles.system.path = mkPath "desktop" "x86_64-linux";
      };
      laptop = {
        hostname = "laptop";
        profiles.system.path = mkPath "laptop" "x86_64-linux";
      };
      work = {
        hostname = "work";
        profiles.system.path = mkPath "work" "x86_64-linux";
      };
      vps = {
        hostname = "vps";
        profiles.system.path = mkPath "vps" "x86_64-linux";
      };
    };
  };

  checks = mapAttrs (_: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;
}
