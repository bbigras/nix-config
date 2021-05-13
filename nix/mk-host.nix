{ inputs
, name
, system
, extraModules ? [ ]
}:
let
  inherit (nixpkgs.lib) pathExists optionalAttrs mapAttrs' nameValuePair;
  inherit (builtins) attrNames readDir;
  inherit (inputs) nixpkgs impermanence home-manager sops-nix emacs-overlay nixpkgs-cdda-mods nur nix-matrix-pinecone;

  config = {
    allowUnfree = true;
    allowAliases = true;
  };

  overlays = map
    (f: import (./overlays + "/${f}"))
    (attrNames (optionalAttrs (pathExists ./overlays) (readDir ./overlays))) ++ [
    (import "${nixpkgs-cdda-mods}")
    emacs-overlay.overlay
    nur.overlay

    (_self: _super: {
      inherit nix-matrix-pinecone;
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
