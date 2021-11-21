{ deploy-rs
, gitignore
, nixpkgs
, ragenix
, emacs-overlay
, nur
, dendrite-demo-pinecone
, emacs-plz
, defmacro-gensym
, emacs-ement-extras
, emacs-ement
, ...
}:

let
  inherit (nixpkgs.lib) composeManyExtensions;
  inherit (builtins) attrNames readDir;
  localOverlays = map
    (f: import (./overlays + "/${f}"))
    (attrNames (readDir ./overlays));
  # dendrite-demo-pinecone2 = dendrite-demo-pinecone.defaultPackage."${system}";
in
composeManyExtensions (localOverlays ++ [
  deploy-rs.overlay
  gitignore.overlay
  # ragenix.overlay
  emacs-overlay.overlay
  nur.overlay

  # (_self: _super: {
  #   dendrite-demo-pinecone = dendrite-demo-pinecone2;
  # })

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
      ement-extras = super.emacsPackages.trivialBuild {
        pname = "ement-extras";
        version = "git";
        src = emacs-ement-extras;

        packageRequires = [
          super.emacsPackages.plz
          super.emacsPackages.ement
          super.emacsPackages.defmacro-gensym
          #super.emacsPackages.ts
        ];

        buildPhase = ''
          runHook preBuild
          make all
          rm *.el
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
        ];
        src = emacs-ement;
      };
    };
  })

])
