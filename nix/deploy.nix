{ self
, deploy-rs
, nixpkgs
, nix-on-droid
, ...
}:
let
  inherit (nixpkgs) lib;
  hosts = (import ./hosts.nix).all;

  genNode = hostName: nixosCfg:
    let
      inherit (hosts.${hostName}) address localSystem;
      inherit (deploy-rs.lib.${localSystem}) activate;
    in
    {
      hostname = address;
      profiles.system.path = activate.nixos nixosCfg;
    };

  pkgs_arch64 = import nixpkgs {
    system = "aarch64-linux";
    inherit (self.nixpkgs."aarch64-linux") overlays config;
  };

  pixel6 = (nix-on-droid.lib.nixOnDroidConfiguration {
    system = "aarch64-linux";
    pkgs = pkgs_arch64;
    config = ../hosts/pixel6;
  }).activationPackage;
in
{
  autoRollback = true;
  magicRollback = true;
  user = "root";
  nodes = lib.mapAttrs genNode self.nixosConfigurations // {
    pixel6 = {
      hostname = "pixel6";

      # to prevent using sudo
      sshUser = "nix-on-droid";
      user = "nix-on-droid";

      profiles.system.path = deploy-rs.lib.aarch64-linux.activate.custom
        pixel6
        (pixel6 + "/activate");
    };
  };
}
