{ self
, deploy-rs
, home-manager
, impermanence
, nixpkgs
, sops-nix
, nur
, nixpkgs-cdda-mods
, emacs-overlay
, ...
}@inputs:
let
  inherit (nixpkgs.lib) pathExists optionalAttrs;
  inherit (builtins) attrNames mapAttrs readDir;

  overlays = [
    nur.overlay
    (import "${nixpkgs-cdda-mods}")
    emacs-overlay.overlay
    (_self: _super: {
      explain-pause-mode = epkgs: epkgs.trivialBuild {
        pname = "explain-pause-mode";
        version = "git";
        src = inputs.explain-pause-mode;
      };
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
    };
  };

  checks = mapAttrs (_: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;
}
