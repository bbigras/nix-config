{
  description = "bbigras's NixOS config";

  nixConfig = {
    extra-trusted-substituters = [
      "https://bbigras-nix-config.cachix.org"
      "https://nix-community.cachix.org"
      "https://nix-on-droid.cachix.org"
      "https://pre-commit-hooks.cachix.org"
      "https://cosmic.cachix.org"
    ];
    extra-trusted-public-keys = [
      "bbigras-nix-config.cachix.org-1:aXL6Q9Oi0jyF79YAKRu17iVNk9HY0p23OZX7FA8ulhU="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "nix-on-droid.cachix.org-1:56snoMJTXmDRC1Ei24CmKoUqvHJ9XCp+nidK7qkMQrU="
      "pre-commit-hooks.cachix.org-1:Pkk3Panw5AW24TOv6kz3PvLhlH8puAsJTBbOPmBo7Rc="
      "cosmic.cachix.org-1:Dya9IyXD4xdBehWjrkPv6rtxpmMdRel02smYzA85dPE="
    ];
  };

  inputs = {
    agenix = {
      url = "github:ryantm/agenix";
      inputs = {
        darwin.follows = "darwin";
        home-manager.follows = "home-manager";
        nixpkgs.follows = "nixpkgs";
        systems.follows = "systems";
      };
    };

    nixos-cosmic = {
      url = "github:lilyinstarlight/nixos-cosmic";
      #url = "github:lilyinstarlight/nixos-cosmic?ref=d903c5edacd867e3f1479098e83df21098961327";
      #inputs.nixpkgs.follows = "nixpkgs";
    };

    truecolor-check = {
      url = "git+https://gist.github.com/fdeaf79e921c2f413f44b6f613f6ad53.git";
      flake = false;
    };

    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs_zed.url = "github:nixos/nixpkgs?rev=d233eb89118bcc22856ac7d511ef56c146567c8b";
    nixpkgs_jj-fzf.url = "github:bbigras/nixpkgs/push-qsnkmvlwnrvm";

    nixos-facter-modules.url = "github:numtide/nixos-facter-modules";

    srvos = {
      url = "github:nix-community/srvos";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-compat.url = "github:edolstra/flake-compat";

    deploy-rs = {
      url = "github:serokell/deploy-rs";
      inputs = {
        flake-compat.follows = "flake-compat";
        nixpkgs.follows = "nixpkgs";
        utils.follows = "flake-utils";
      };
    };

    flake-parts.url = "github:hercules-ci/flake-parts";

    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };

    gemoji = {
      url = "github:github/gemoji";
      flake = false;
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    impermanence.url = "github:nix-community/impermanence";

    lanzaboote = {
      url = "github:nix-community/lanzaboote";
      inputs = {
        flake-compat.follows = "flake-compat";
        flake-parts.follows = "flake-parts";
        nixpkgs.follows = "nixpkgs";
        pre-commit-hooks-nix.follows = "git-hooks";
      };
    };

    nix-fast-build = {
      url = "github:Mic92/nix-fast-build";
      inputs = {
        flake-parts.follows = "flake-parts";
        nixpkgs.follows = "nixpkgs";
      };
    };

    nix-index-database = {
      url = "github:Mic92/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-hardware.url = "github:NixOS/nixos-hardware";

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs = {
        flake-compat.follows = "flake-compat";
        nixpkgs.follows = "nixpkgs";
      };
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nur.url = "nur";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # copilot_pkgs.url = "github:bbigras/nixpkgs/copilot";
    # copilot-node-server_pkgs.url = "github:bbigras/nixpkgs/copilot-node-server";

    envrc = {
      url = "github:siddharthverma314/envrc";
      flake = false;
    };

    defmacro-gensym = {
      url = "gitlab:akater/defmacro-gensym";
      flake = false;
    };

    nix-on-droid = {
      url = "github:t184256/nix-on-droid";
      inputs.home-manager.follows = "home-manager";
    };

    catppuccin.url = "github:catppuccin/nix";

    systems.url = "github:nix-systems/default";

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{ self, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } (
      toplevel@{ withSystem, ... }:
      {
        imports = [
          inputs.git-hooks.flakeModule
          inputs.treefmt-nix.flakeModule
        ];
        systems = [
          "aarch64-linux"
          "x86_64-linux"
        ];
        perSystem =
          ctx@{
            config,
            self',
            inputs',
            pkgs,
            system,
            ...
          }:
          {
            _module.args.pkgs = import inputs.nixpkgs {
              localSystem = system;
              overlays = [ self.overlays.default ];
              config = {
                allowUnfree = false;
                allowAliases = true;
                allowUnfreePredicate =
                  pkg:
                  builtins.elem (pkgs.lib.getName pkg) [
                    "keet"
                    "steam"
                    "steam-original"
                    "steam-run"
                    "steam-unwrapped"
                  ];
              };
            };

            devShells = import ./nix/dev-shell.nix ctx;

            packages = import ./nix/packages.nix toplevel ctx;

            pre-commit = {
              check.enable = true;
              settings.hooks = {
                actionlint.enable = true;
                nil.enable = true;
                shellcheck.enable = true;
                statix.enable = true;
                treefmt.enable = true;
              };
            };

            treefmt = {
              projectRootFile = "flake.nix";
              flakeCheck = false; # Covered by git-hooks check
              programs = {
                nixfmt.enable = true;
                shfmt = {
                  enable = true;
                  indent_size = 0;
                };
              };
            };
          };

        flake = {
          hosts = import ./nix/hosts.nix;

          darwinConfigurations = import ./nix/darwin.nix toplevel;
          homeConfigurations = import ./nix/home-manager.nix toplevel;
          nixosConfigurations = import ./nix/nixos.nix toplevel;
          nixondroidConfigurations = import ./nix/nix-on-droid.nix toplevel;

          deploy = import ./nix/deploy.nix toplevel;

          overlays = import ./nix/overlay.nix toplevel;
        };
      }
    );
}
