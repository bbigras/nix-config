{ nixpkgs
, nix-on-droid
, nur
, emacs-overlay
, stylix
, ...
}:
let
  inherit (nixpkgs) lib;
  hosts = (import ./hosts.nix).nix-on-droid;

  pkgs = import nixpkgs {
    system = "aarch64-linux"; # FIXME
    overlays = [
      emacs-overlay.overlay
    ];
    config.allowUnfree = true;
    config.allowAliases = true;
  };

  genConfiguration = hostname: _:
    nix-on-droid.lib.nixOnDroidConfiguration {
      inherit pkgs;
      modules = [
        (../hosts + "/${hostname}")
      ];
      extraSpecialArgs = {
        inherit nur stylix;
      };
    };
in
lib.mapAttrs genConfiguration hosts
