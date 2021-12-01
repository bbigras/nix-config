{
  description = "bbigras's NixOS config";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    deploy-rs = {
      url = "github:serokell/deploy-rs";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        utils.follows = "utils";
        flake-compat.follows = "flake-compat";
      };
    };

    utils.url = "github:numtide/flake-utils";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    impermanence.url = "github:nix-community/impermanence";

    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "utils";
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ragenix = {
      url = "github:yaxitech/ragenix";
      inputs.flake-utils.follows = "utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-hardware.url = "nixos-hardware";
    nur.url = "nur";
    emacs-overlay.url = "github:nix-community/emacs-overlay";

    emacs-plz = {
      url = "github:alphapapa/plz.el";
      flake = false;
    };

    emacs-ement-extras = {
      url = "gitlab:akater/emacs-ement-extras/release";
      flake = false;
    };

    defmacro-gensym = {
      url = "gitlab:akater/defmacro-gensym";
      flake = false;
    };

    emacs-ement = {
      url = "github:alphapapa/ement.el";
      flake = false;
    };

    dendrite-demo-pinecone = {
      url = "github:bbigras/dendrite-demo-pinecone";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "utils";
        flake-compat.follows = "flake-compat";
        pre-commit-hooks.follows = "pre-commit-hooks";
      };
    };

    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    templates.url = "github:NixOS/templates";

    nix-on-droid = {
      url = "github:t184256/nix-on-droid";
      inputs = {
        flake-utils.follows = "utils";
        home-manager.follows = "home-manager";
        nixpkgs.follows = "nixpkgs";
      };
    };
  };

  outputs = { self, nixpkgs, utils, ... }@inputs:
    {
      deploy = import ./nix/deploy.nix inputs;
      overlay = import ./nix/overlay.nix inputs;
    }
    // utils.lib.eachDefaultSystem (system: {
      checks = import ./nix/checks.nix inputs system;

      devShell = import ./nix/dev-shell.nix inputs system;

      nixpkgs = import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
        config.allowUnfree = true;
      };

      packages.hosts = import ./nix/join-host-drvs.nix inputs system;

      defaultPackage = self.packages.${system}.hosts;
    });
}
