{ self, ... }:

system:

let
  inherit (self.pkgs.${system}) lib linkFarm;

  nixosDrvs = lib.mapAttrs (_: nixos: nixos.config.system.build.toplevel) self.nixosConfigurations;
  homeDrvs = lib.mapAttrs (_: home: home.activationPackage) self.homeConfigurations;
  # darwinDrvs = lib.mapAttrs (_: darwin: darwin.system) self.darwinConfigurations;

  hostDrvs' = nixosDrvs // homeDrvs;
  hostDrvs = lib.mapAttrs (_: drv: drv // { allowSubstitutes = true; }) hostDrvs';

  structuredHostDrvs = lib.mapAttrsRecursiveCond
    (hostAttr: !(hostAttr ? "type" && (lib.elem hostAttr.type [ "homeManager" "nixos" ])))
    (path: _: hostDrvs.${lib.last path})
    self.hosts;

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
