{ self
, nixpkgs
, base16-schemes
, nix-on-droid
, nur
, stylix
, ...
}:
let
  inherit (nixpkgs) lib;

  genConfiguration = hostname: { hostPlatform, ... }:
    nix-on-droid.lib.nixOnDroidConfiguration {
      pkgs = self.pkgs.${hostPlatform};
      modules = [
        (../hosts + "/${hostname}")
      ];
      extraSpecialArgs = {
        inherit nur stylix base16-schemes;
      };
    };
in
lib.mapAttrs
  genConfiguration
  (lib.filterAttrs (_: host: host.type == "nix-on-droid") self.hosts)
