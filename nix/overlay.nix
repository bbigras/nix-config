{ deploy-rs
, emacs-overlay
, nix
, nixpkgs
, nur
, dendrite-demo-pinecone
, emacs-plz
, defmacro-gensym
, emacs-ement
, org-tufte
, org-modern-indent
, codemetrics
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

  dendrite-demo-pinecone2 = dendrite-demo-pinecone.defaultPackage."x86_64-linux";
in
localOverlays // {
  default = lib.composeManyExtensions ((lib.attrValues localOverlays) ++ [
    nix.overlays.default
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
        plz = super.emacsPackages.trivialBuild {
          pname = "plz";
          version = "git";
          src = emacs-plz;

          postPatch = ''
            substituteInPlace ./plz.el --replace 'plz-curl-program "curl"' 'plz-curl-program "${super.curl}/bin/curl"'
          '';
        };
      };
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
        ement = super.emacsPackages.trivialBuild {
          pname = "ement";
          version = "git";
          packageRequires = [
            super.emacsPackages.plz
            super.emacsPackages.ts
            super.emacsPackages.cl-lib
            super.emacsPackages.taxy
            super.emacsPackages.taxy-magit-section
            super.emacsPackages.svg-lib
            super.emacsPackages.persist
          ];
          src = emacs-ement;
        };
      };
    })

    (_self: super: {
      emacsPackages = super.emacsPackages // {
        codemetrics = super.emacsPackages.trivialBuild {
          pname = "codemetrics";
          version = "git";
          packageRequires = [
            # super.emacsPackages.plz
            # super.emacsPackages.ts
            # super.emacsPackages.cl-lib
            # super.emacsPackages.taxy
            # super.emacsPackages.taxy-magit-section
            # super.emacsPackages.svg-lib
            # super.emacsPackages.persist
            super.emacsPackages.s
            super.emacsPackages.tree-sitter
          ];
          src = codemetrics;
        };
      };
    })

    (_self: super: {
      emacsPackages = super.emacsPackages // {
        org-tufte = super.emacsPackages.trivialBuild {
          pname = "org-tufte";
          version = "git";
          packageRequires = [
            # super.emacsPackages.plz
            # super.emacsPackages.ts
            # super.emacsPackages.cl-lib
            # super.emacsPackages.taxy
            # super.emacsPackages.taxy-magit-section
            # super.emacsPackages.svg-lib
            # super.emacsPackages.persist
            super.emacsPackages.s
          ];
          src = org-tufte;
        };
      };
    })


    (_self: super: {
      emacsPackages = super.emacsPackages // {
        org-modern-indent = super.emacsPackages.trivialBuild {
          pname = "org-modern-indent";
          version = "git";
          packageRequires = [
            # super.emacsPackages.plz
            # super.emacsPackages.ts
            # super.emacsPackages.cl-lib
            # super.emacsPackages.taxy
            # super.emacsPackages.taxy-magit-section
            # super.emacsPackages.svg-lib
            # super.emacsPackages.persist
            super.emacsPackages.compat
          ];
          src = org-modern-indent;
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
