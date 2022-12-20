{ self, ... }:

system:

let
  inherit (self.pkgs.${system}) lib linkFarm;

  hosts = import ./hosts.nix;

  nixosDrvs = lib.mapAttrs (_: nixos: nixos.config.system.build.toplevel) self.nixosConfigurations;
  homeDrvs = lib.mapAttrs (_: home: home.activationPackage) self.homeConfigurations;
  darwinDrvs = lib.mapAttrs (_: darwin: darwin.system) self.darwinConfigurations;
  nixondroidDrvs = lib.mapAttrs (_: home: home.activationPackage) self.nixondroidConfigurations;

  hostDrvs = nixosDrvs // homeDrvs // darwinDrvs // nixondroidDrvs;

  structuredHostDrvs = lib.mapAttrsRecursiveCond
    (as: !(as ? "type" && (lib.elem as.type [ "darwin" "home-manager" "nixos" "nix-on-droid" ])))
    (path: _: hostDrvs.${lib.last path})
    hosts;

  structuredHostFarms = lib.mapAttrsRecursiveCond
    (as: !(lib.any lib.isDerivation (lib.attrValues as)))
    (path: values:
      (linkFarm
        (lib.concatStringsSep "-" path)
        (lib.mapAttrsToList (name: path: { inherit name path; }) values)) //
      values
    )
    structuredHostDrvs;
in
structuredHostFarms
