{ deploy-rs
, emacs-overlay
, nixpkgs
, nur
, defmacro-gensym
, combobulate
, ...
}:

let
  inherit (nixpkgs) lib;
  localOverlays =
    lib.mapAttrs'
      (f: _: lib.nameValuePair
        (lib.removeSuffix ".nix" f)
        (import (./overlays + "/${f}")))
      (builtins.readDir ./overlays);
in
localOverlays // {
  default = lib.composeManyExtensions ((lib.attrValues localOverlays) ++ [
    deploy-rs.overlay
    emacs-overlay.overlay
    nur.overlay

    (self: super: {
      ripgrep = super.ripgrep.overrideAttrs (_: { doCheck = false; });
    })

    (_self: super: {
      emacsPackages = super.emacsPackages // {
        defmacro-gensym = super.emacsPackages.trivialBuild {
          pname = "defmacro-gensym";
          version = "git";
          src = defmacro-gensym;
          buildPhase = ''
            runHook preBuild
            make default
            runHook postBuild
          '';
        };
      };
    })
    (_self: super: {
      emacsPackages = super.emacsPackages // {
        combobulate = (super.emacsPackagesFor super.emacs-git).trivialBuild {
          pname = "combobulate";
          version = "git";
          src = combobulate;
        };
      };
    })
  ]);
}
