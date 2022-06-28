{
  description = "bbigras's NixOS config";
  nixConfig.substituters = [
    "https://cache.nixos.org"
    "https://bbigras-nix-config.cachix.org"
    # "https://cache.ngi0.nixos.org"
    "https://dendrite-demo-pinecone.cachix.org"
    "https://nix-community.cachix.org"
    "https://nix-on-droid.cachix.org"
    "https://pre-commit-hooks.cachix.org"
  ];
  nixConfig.trusted-public-keys = [
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    "bbigras-nix-config.cachix.org-1:aXL6Q9Oi0jyF79YAKRu17iVNk9HY0p23OZX7FA8ulhU="
    # "cache.ngi0.nixos.org-1:KqH5CBLNSyX184S9BKZJo1LxrxJ9ltnY2uAs5c/f1MA="
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    "dendrite-demo-pinecone.cachix.org-1:qgybhOM1X0JikTrvpYo1HwtsXT2ee+6ajbmCjCns4yI="
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    "nix-on-droid.cachix.org-1:56snoMJTXmDRC1Ei24CmKoUqvHJ9XCp+nidK7qkMQrU="
    "pre-commit-hooks.cachix.org-1:Pkk3Panw5AW24TOv6kz3PvLhlH8puAsJTBbOPmBo7Rc="
  ];

  inputs = {
    nixgl = {
      url = "github:guibou/nixgl";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };

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
      inputs.utils.follows = "utils";
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
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "utils";
    };

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

      overlays = {
        default = import ./nix/overlay.nix inputs;
        lite = import ./nix/mask-large-drvs.nix;
      };

      homeConfigurations = import ./nix/home-manager.nix inputs;

      nixosConfigurations = import ./nix/nixos.nix inputs;
    }
    // utils.lib.eachSystem [ "aarch64-linux" "x86_64-linux" ] (system: {
      checks = import ./nix/checks.nix inputs system;

      devShells.default = import ./nix/dev-shell.nix inputs system;

      packages = {
        default = self.packages.${system}.all-hosts;
      } // (import ./nix/host-drvs.nix inputs system);

      nixpkgs = import nixpkgs {
        inherit system;
        overlays = [
          self.overlays.default
          # self.overlays.lite
        ];
        config.allowUnfree = true;
        config.allowAliases = true;
      };
    });
}
