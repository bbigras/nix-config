{
  description = "bbigras's NixOS config";

  inputs = {
    deploy-rs = {
      url = "github:serokell/deploy-rs";
      inputs = {
        naersk.follows = "naersk";
        nixpkgs.follows = "nixpkgs";
        utils.follows = "flake-utils";
      };
    };

    flake-utils.url = "github:numtide/flake-utils";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    impermanence.url = "github:nix-community/impermanence";

    naersk = {
      url = "github:nmattia/naersk";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.url = "nixpkgs/nixos-unstable";

    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-hardware.url = "nixos-hardware";
    nur.url = "nur";
    emacs-overlay.url = "github:nix-community/emacs-overlay";

    nixpkgs-cdda-mods = {
      url = "github:mnacamura/nixpkgs-cdda-mods";
      flake = false;
    };

    nix-matrix-pinecone = {
      url = "github:bbigras/nix-matrix-pinecone";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };

    nix-on-droid = {
      # url = "/home/bbigras/src/nix-on-droid";
      url = "github:bbigras/nix-on-droid/pixel2";
      inputs = {
        flake-utils.follows = "flake-utils";
        home-manager.follows = "home-manager";
        nixpkgs.follows = "nixpkgs";
      };
    };
  };

  # FIXME: I can't η-reduce this for some reason
  outputs = args: import ./nix/outputs.nix args;
}
