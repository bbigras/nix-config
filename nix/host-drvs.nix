{ self, nix-on-droid, deploy-rs, nixpkgs, nur, ... }:

system:

let
  inherit (self.nixpkgs.${system}) lib linkFarm;

  pkgs_arch64 = import nixpkgs {
    system = "aarch64-linux";
    inherit (self.nixpkgs."aarch64-linux") overlays config;
  };

  pixel6 = (nix-on-droid.lib.nixOnDroidConfiguration {
    system = "aarch64-linux";
    pkgs = pkgs_arch64;
    config = ../hosts/pixel6;
    extraSpecialArgs = {
      inherit nur;
    };
  }).activationPackage;

  nixosDrvs = lib.mapAttrs (_: nixos: nixos.config.system.build.toplevel) self.nixosConfigurations;
  homeDrvs = lib.mapAttrs (_: home: home.activationPackage) self.homeConfigurations;
  deployDrvs = lib.mapAttrs (_: deploy: deploy.profiles.system.path) {
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

  hostDrvs = nixosDrvs // homeDrvs // deployDrvs;
in
hostDrvs // {
  all-hosts = linkFarm "all-hosts" (lib.mapAttrsToList (name: path: { inherit name path; }) hostDrvs);
}
