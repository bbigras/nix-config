{
  description = "bbigras's NixOS config";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";

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

    dendrite-demo-pinecone = {
      url = "github:bbigras/dendrite-demo-pinecone/2e344f974f67c63f7383c3bd58e61ca5e2fd4b9c";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };

    nix-on-droid = {
      url = "github:t184256/nix-on-droid";
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
