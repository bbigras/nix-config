{ nixpkgs
, base16-schemes
, nix-on-droid
, emacs-overlay
, stylix
, rycee-nur-expressions
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
        inherit rycee-nur-expressions stylix base16-schemes;
      };
    };
in
lib.mapAttrs genConfiguration hosts
