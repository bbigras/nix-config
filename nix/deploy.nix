{ self
, deploy-rs
, nixpkgs
, ...
}:
let
  inherit (nixpkgs) lib;

  genNode = hostName: nixosCfg:
    let
      inherit (self.hosts.${hostName}) address hostPlatform remoteBuild;
      inherit (deploy-rs.lib.${hostPlatform}) activate;
    in
    {
      inherit remoteBuild;
      hostname = address;
      profiles.system.path = activate.nixos nixosCfg;
    };

  genNixOnDroid = hostName: nixosCfg:
    let
      inherit (self.hosts.${hostName}) address hostPlatform remoteBuild;
      inherit (deploy-rs.lib.${hostPlatform}) activate;

      pixel6 = nixosCfg.activationPackage;

    in
    {
      inherit remoteBuild;
      hostname = address;

      # to prevent using sudo
      sshUser = "nix-on-droid";
      user = "nix-on-droid";

      profiles.system.path = activate.custom
        pixel6
        (pixel6 + "/activate");
    };
in
{
  # XXX: auto-rollback is too noisy since any service failing will cause it to
  # go haywire.
  autoRollback = false;
  magicRollback = true;
  user = "root";
  nodes = lib.mapAttrs genNode self.nixosConfigurations //
    lib.mapAttrs genNixOnDroid self.nixondroidConfigurations;
}
