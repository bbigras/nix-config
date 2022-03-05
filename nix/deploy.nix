{ self
, deploy-rs
, home-manager
, impermanence
, nixpkgs
, ragenix
, templates
, sops-nix
, ...
}@inputs:
let
  inherit (nixpkgs.lib) mapAttrs' nameValuePair nixosSystem;

  genModules = hostName: system: [
    # ragenix.nixosModules.age
    impermanence.nixosModules.impermanence
    home-manager.nixosModules.home-manager
    sops-nix.nixosModules.sops

    {
      nixpkgs = {
        localSystem.system = system;
        inherit (self.nixpkgs.${system}) overlays config;
      };

      nix.registry = {
        templates.flake = templates;
        nixpkgs.flake = nixpkgs;
      };

      networking.hosts = mapAttrs' (n: v: nameValuePair v.address [ n ]) (import ./hosts.nix);
    }

    (../hosts + "/${hostName}")
  ];

  mkHost = hostName: system: nixpkgs.lib.nixosSystem {
    modules = genModules hostName system;
    specialArgs.inputs = inputs;
  };

  mkActivation = hostName: localSystem:
    deploy-rs.lib.${localSystem}.activate.nixos (mkHost hostName localSystem);

  pkgs_arch64 = import nixpkgs {
    system = "aarch64-linux";
    # localSystem.system = "aarch64-linux";
    inherit (self.nixpkgs."aarch64-linux") overlays config;
  };

  pixel2 = (inputs.nix-on-droid.lib.nixOnDroidConfiguration {
    system = "aarch64-linux";
    pkgs = pkgs_arch64;
    config = ../hosts/pixel2;
  }).activationPackage;
in
{
  autoRollback = true;
  magicRollback = true;
  user = "root";
  nodes = builtins.mapAttrs
    (host: info: {
      hostname = info.address;
      profiles.system.path = mkActivation host info.localSystem;
    })
    (import ./hosts.nix) //
  {
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
}
