{ deploy-rs
, emacs-overlay
, nixpkgs
, nur
, dendrite-demo-pinecone
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

  dendrite-demo-pinecone2 = dendrite-demo-pinecone.packages."x86_64-linux".dendrite;
in
localOverlays // {
  default = lib.composeManyExtensions ((lib.attrValues localOverlays) ++ [
    deploy-rs.overlay
    emacs-overlay.overlay
    nur.overlay

    (_self: _super: {
      dendrite-demo-pinecone = dendrite-demo-pinecone2;
    })

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
