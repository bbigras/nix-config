# Configuration wiring using nixos-unified helpers
# Uses mkLinuxSystem/mkMacosSystem/mkHomeConfiguration for standardized setup
{
  self,
  ...
}:
let
  inherit (self.nixos-unified.lib) mkLinuxSystem;

  # We use home-manager = false because we have our own customized home-manager setup
  # in modules/nixos/default.nix and modules/darwin/default.nix
  mkNixos = mkLinuxSystem { home-manager = false; };
in
{
  flake = {
    nixosConfigurations = {
      desktop = mkNixos (self + "/configurations/nixos/desktop");
      laptop = mkNixos (self + "/configurations/nixos/laptop");
      work = mkNixos (self + "/configurations/nixos/work");
    };

    darwinConfigurations = {
    };

    homeConfigurations = {
    };
  };
}
