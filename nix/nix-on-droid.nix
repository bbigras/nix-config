{ nixpkgs
, nix-on-droid
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
    };
in
lib.mapAttrs genConfiguration hosts
