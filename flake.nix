{
  description = "bbigras's NixOS config";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    nixos-hardware.url = "nixos-hardware";
    nur.url = "nur";

    flake-utils.url = "github:numtide/flake-utils";

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:rycee/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    deploy-rs = {
      url = "github:serokell/deploy-rs";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        utils.follows = "flake-utils";
      };
    };

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    impermanence.url = "github:nix-community/impermanence";

    nixpkgs-cdda-mods = {
      url = "github:mnacamura/nixpkgs-cdda-mods";
      flake = false;
    };

    nix-matrix-pinecone = {
      url = "github:bbigras/nix-matrix-pinecone";
      flake = false;
    };

    nix-direnv = {
      url = "github:nix-community/nix-direnv/760ce16e6c5336dc816e4112dcc8ddcdcadd7e51";
      flake = false;
    };
  };

  # FIXME: I can't η-reduce this for some reason
  outputs = args: import ./nix/outputs.nix args;
}
