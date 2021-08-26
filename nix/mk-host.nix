{ inputs
, name
, system
, extraModules ? [ ]
}:
let
  inherit (nixpkgs.lib) pathExists optionalAttrs mapAttrs' nameValuePair;
  inherit (builtins) attrNames readDir;
  inherit (inputs) nixpkgs impermanence home-manager sops-nix emacs-overlay nixpkgs-cdda-mods nur dendrite-demo-pinecone;

  dendrite-demo-pinecone2 = dendrite-demo-pinecone.defaultPackage."${system}";

  config = {
    allowUnfree = true;
    allowAliases = true;
    joypixels.acceptLicense = true;
  };

  overlays = map
    (f: import (./overlays + "/${f}"))
    (attrNames (optionalAttrs (pathExists ./overlays) (readDir ./overlays))) ++ [
    (import "${nixpkgs-cdda-mods}")
    emacs-overlay.overlay
    nur.overlay

    (_self: _super: {
      dendrite-demo-pinecone = dendrite-demo-pinecone2;
    })

    (self: super: {
      emacsPackages = super.emacsPackages // {
        plz = super.emacsPackages.trivialBuild {
          pname = "plz";
          version = "git";
          src = inputs.emacs-plz;
        };
      };
    })

    (self: super: {
      emacsPackages = super.emacsPackages // {
        ement = super.emacsPackages.trivialBuild {
          pname = "ement";
          version = "git";
          packageRequires = [
            super.emacsPackages.plz
            super.emacsPackages.ts
          ];
          src = inputs.emacs-ement;
        };
      };
    })
  ];
in
nixpkgs.lib.nixosSystem {
  inherit system;

  modules = [
    ({ nixpkgs = { inherit config overlays; }; })
    impermanence.nixosModules.impermanence
    home-manager.nixosModules.home-manager
    sops-nix.nixosModules.sops

    ({
      networking.hosts = mapAttrs' (n: v: nameValuePair v.hostname [ n ]) (import ./hosts.nix);
    })

    (../hosts + "/${name}")
  ] ++ extraModules;

  specialArgs.inputs = inputs;
}
