{ withSystem, inputs, ... }:

let
  inherit (inputs) self nix-on-droid nur catppuccin nixpkgs;
  inherit (nixpkgs) lib;

  genConfiguration = hostname: { hostPlatform, ... }:
    withSystem hostPlatform ({ pkgs, ... }:
      nix-on-droid.lib.nixOnDroidConfiguration {
        inherit pkgs;
        modules = [
          (../hosts + "/${hostname}")
        ];
        extraSpecialArgs = {
          inherit nur catppuccin;
        };
      });
in
lib.mapAttrs
  genConfiguration
  (lib.filterAttrs (_: host: host.type == "nix-on-droid") self.hosts)
