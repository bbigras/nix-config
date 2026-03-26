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
        (final: super: {
          emacs-lsp-booster =
            final.callPackage "${inputs.nixpkgs}/pkgs/by-name/em/emacs-lsp-booster/package.nix"
              {
                inherit (super) emacs;
              };
        })
        (final: _super: {
          veilid = final.callPackage "${inputs.nixpkgs_veilid}/pkgs/by-name/ve/veilid/package.nix" { };
        })
        (final: _super: {
          resticprofile = final.callPackage "${inputs.nixpkgs_resticprofile}/pkgs/by-name/re/resticprofile/package.nix" { };
        })
        (_self: super: {
          emacs.pkgs = super.emacs.pkgs // {
            eglot-multi-preset = super.emacs.pkgs.trivialBuild {
              pname = "eglot-multi-preset";
              version = "git";
              src = inputs.eglot-multi-preset;
            };
            eglot-x = super.emacs.pkgs.trivialBuild {
              pname = "eglot-x";
              version = "git";
              src = inputs.eglot-x;
            };
            treesit-sexp = super.emacs.pkgs.trivialBuild {
              pname = "treesit-sexp";
              version = "git";
              src = inputs.treesit-sexp;
            };
            majutsu = super.emacs.pkgs.trivialBuild {
              pname = "majutsu";
              version = "git";
              src = inputs.majutsu;
              packageRequires = [
                super.emacs.pkgs.magit
              ];
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
