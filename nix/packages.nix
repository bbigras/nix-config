{ config, ... }:
{
  inputs',
  pkgs,
  system,
  ...
}:

let
  inherit (config) flake;
  inherit (pkgs) lib linkFarm;

  nixosDrvs = lib.mapAttrs (_: nixos: nixos.config.system.build.toplevel) flake.nixosConfigurations;
  homeDrvs = lib.mapAttrs (_: home: home.activationPackage) flake.homeConfigurations;
  darwinDrvs = lib.mapAttrs (_: darwin: darwin.system) flake.darwinConfigurations;
  nixondroidDrvs = lib.mapAttrs (_: home: home.activationPackage) flake.nixondroidConfigurations;
  hostDrvs = nixosDrvs // homeDrvs // darwinDrvs // nixondroidDrvs;

  compatHosts = lib.filterAttrs (_: host: host.hostPlatform == system) flake.hosts;
  compatHostDrvs = lib.mapAttrs (name: _: hostDrvs.${name}) compatHosts;

  compatHostsFarm = linkFarm "hosts-${system}" (
    lib.mapAttrsToList (name: path: { inherit name path; }) compatHostDrvs
  );
in
compatHostDrvs
// (lib.optionalAttrs (compatHosts != { }) {
  default = compatHostsFarm;
})
// {
  inherit (pkgs) cachix jq nix-fast-build;
}
