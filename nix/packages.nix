{ self, nix, ... }:

localSystem:

let
  inherit (self.pkgs.${localSystem}) lib linkFarm;

  nixosDrvs = lib.mapAttrs (_: nixos: nixos.config.system.build.toplevel) self.nixosConfigurations;
  homeDrvs = lib.mapAttrs (_: home: home.activationPackage) self.homeConfigurations;
  darwinDrvs = lib.mapAttrs (_: darwin: darwin.system) self.darwinConfigurations;
  nixondroidDrvs = lib.mapAttrs (_: home: home.activationPackage) self.nixondroidConfigurations;

  hostDrvs = nixosDrvs // homeDrvs // darwinDrvs // nixondroidDrvs;

  structuredHostDrvs = lib.mapAttrsRecursiveCond
    (hostAttr: !(hostAttr ? "type" && (lib.elem hostAttr.type [ "darwin" "homeManager" "nixos" "nix-on-droid" ])))
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

  defaultHostFarm =
    if builtins.hasAttr localSystem structuredHostFarms
    then { default = self.packages.${localSystem}.${localSystem}; }
    else { };
in
structuredHostFarms // defaultHostFarm // {
  inherit (nix.packages.${localSystem}) nixBinaryTarball;
}
