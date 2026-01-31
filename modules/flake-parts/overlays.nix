# Overlay configuration
{ inputs, ... }:
let
  inherit (inputs.nixpkgs) lib;

  localOverlays = lib.mapAttrs' (
    f: _: lib.nameValuePair (lib.removeSuffix ".nix" f) (import (../../overlays + "/${f}"))
  ) (builtins.readDir ../../overlays);
in
{
  flake.overlays = localOverlays // {
    default = lib.composeManyExtensions (
      [
        inputs.agenix.overlays.default
        inputs.emacs-overlay.overlay
        inputs.nur.overlays.default
        (final: _: {
          inherit (inputs.nix-fast-build.packages.${final.stdenv.hostPlatform.system}) nix-fast-build;
        })
        (_self: super: {
          emacs.pkgs = super.emacs.pkgs // {
            treesit-sexp = super.emacs.pkgs.trivialBuild {
              pname = "treesit-sexp";
              version = "git";
              src = inputs.treesit-sexp;
            };
          };
        })
      ]
      ++ (lib.attrValues localOverlays)
    );
  };

  perSystem =
    { system, ... }:
    {
      _module.args.pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [ inputs.self.overlays.default ];
        config = {
          allowUnfree = true;
          allowAliases = true;
        };
      };
    };
}
