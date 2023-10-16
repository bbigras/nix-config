{
  description = "bbigras's NixOS config";

  nixConfig = {
    extra-substituters = [
      "https://bbigras-nix-config.cachix.org"
      "https://nix-community.cachix.org"
      "https://nix-on-droid.cachix.org"
      "https://pre-commit-hooks.cachix.org"
    ];
    extra-trusted-public-keys = [
      "bbigras-nix-config.cachix.org-1:aXL6Q9Oi0jyF79YAKRu17iVNk9HY0p23OZX7FA8ulhU="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "nix-on-droid.cachix.org-1:56snoMJTXmDRC1Ei24CmKoUqvHJ9XCp+nidK7qkMQrU="
      "pre-commit-hooks.cachix.org-1:Pkk3Panw5AW24TOv6kz3PvLhlH8puAsJTBbOPmBo7Rc="
    ];
  };

  inputs = {
    base16-schemes = {
      url = "github:tinted-theming/base16-schemes";
      flake = false;
    };

    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    deploy-rs = {
      url = "github:serokell/deploy-rs";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        utils.follows = "flake-utils";
        flake-compat.follows = "flake-compat";
      };
    };

    flake-utils.url = "github:numtide/flake-utils";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    impermanence.url = "github:nix-community/impermanence";

    lanzaboote = {
      url = "github:nix-community/lanzaboote";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-utils.follows = "flake-utils";
      inputs.pre-commit-hooks-nix.follows = "pre-commit-hooks";
    };

    nix-index-database = {
      url = "github:Mic92/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.darwin.follows = "darwin";
      inputs.home-manager.follows = "home-manager";
    };

    nixos-hardware.url = "nixos-hardware";
    rycee-nur-expressions = {
      url = "gitlab:rycee/nur-expressions";
      flake = false;
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };

    defmacro-gensym = {
      url = "gitlab:akater/defmacro-gensym";
      flake = false;
    };
    combobulate = {
      url = "github:mickeynp/combobulate";
      flake = false;
    };

    stylix = {
      url = "github:danth/stylix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
      inputs.flake-compat.follows = "flake-compat";
    };

    templates.url = "github:NixOS/templates";
    nix-on-droid = {
      url = "github:t184256/nix-on-droid";
      inputs.home-manager.follows = "home-manager";
    };
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      forAllSystems = nixpkgs.lib.genAttrs [ "aarch64-linux" "x86_64-linux" ];
    in
    {
      hosts = import ./nix/hosts.nix;

      pkgs = forAllSystems (localSystem: import nixpkgs {
        inherit localSystem;
        overlays = [ self.overlays.default ];
        config.allowUnfree = true;
        config.allowAliases = true;
        config.nvidia.acceptLicense = true;
      });

      checks = forAllSystems (import ./nix/checks.nix inputs);
      devShells = forAllSystems (import ./nix/dev-shell.nix inputs);
      overlays = import ./nix/overlay.nix inputs;
      packages = forAllSystems (import ./nix/packages.nix inputs);

      deploy = import ./nix/deploy.nix inputs;
      darwinConfigurations = import ./nix/darwin.nix inputs;
      homeConfigurations = import ./nix/home-manager.nix inputs;
      nixosConfigurations = import ./nix/nixos.nix inputs;
      nixondroidConfigurations = import ./nix/nix-on-droid.nix inputs;
    };
}
