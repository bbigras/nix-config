{ nixpkgs
, nix-on-droid
, nur
, ...
}:
let
  inherit (nixpkgs) lib;
  hosts = (import ./hosts.nix).nix-on-droid.all;

  genConfiguration = hostname: _:
    nix-on-droid.lib.nixOnDroidConfiguration {
      modules = [
        (../hosts + "/${hostname}")
      ];
      extraSpecialArgs = {
        inherit nur;
      };
    };
in
lib.mapAttrs genConfiguration hosts
