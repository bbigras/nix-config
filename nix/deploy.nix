{ self
, deploy-rs
, home-manager
, impermanence
, nixpkgs
, sops-nix
, nur
, nixpkgs-cdda-mods
, emacs-overlay
, nix-on-droid
, ...
}@inputs:
let
  inherit (nixpkgs.lib) pathExists optionalAttrs;
  inherit (builtins) attrNames mapAttrs readDir;

  overlays = [
    nur.overlay
    (import "${nixpkgs-cdda-mods}")
    emacs-overlay.overlay
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
  pixel2 = (nix-on-droid.lib.aarch64-linux.nix-on-droid { config = ../hosts/pixel2; } ).activationPackage;
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
      pixel2 = {
        hostname = "pixel2";

        # to prevent using sudo
        sshUser = "nix-on-droid";
        user = "nix-on-droid";

        profiles.nix-on-droid.path = deploy-rs.lib.aarch64-linux.activate.custom
          pixel2
          (pixel2 + "/activate");
      };
    };
  };

  checks = mapAttrs (_: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;
}
