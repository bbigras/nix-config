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
      inherit (hosts.${hostName}) address hostPlatform remoteBuild;
      inherit (deploy-rs.lib.${hostPlatform}) activate;
    in
    {
      inherit remoteBuild;
      hostname = address;
      profiles.system.path = activate.nixos nixosCfg;
    };

  pkgs_arch64 = import nixpkgs {
    system = "aarch64-linux";
  };

  pixel6 = (nix-on-droid.lib.nixOnDroidConfiguration {
    pkgs = pkgs_arch64;
    modules = [ ../hosts/pixel6 ];
  }).activationPackage;
in
{
  # XXX: auto-rollback is too noisy since any service failing will cause it to
  # go haywire.
  autoRollback = false;
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
