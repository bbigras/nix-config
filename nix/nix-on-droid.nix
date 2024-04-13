{ self
, nixpkgs
, nix-on-droid
, nur
, catppuccin
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
        inherit nur catppuccin;
      };
    };
in
lib.mapAttrs
  genConfiguration
  (lib.filterAttrs (_: host: host.type == "nix-on-droid") self.hosts)
