{
  description = "bbigras's NixOS config";

  nixConfig = {
    extra-substituters = [
      "https://bbigras-nix-config.cachix.org"
      "https://nix-community.cachix.org"
      "https://nix-on-droid.cachix.org"
      "https://pre-commit-hooks.cachix.org"
      "https://cache.lix.systems"
    ];
    extra-trusted-public-keys = [
      "bbigras-nix-config.cachix.org-1:aXL6Q9Oi0jyF79YAKRu17iVNk9HY0p23OZX7FA8ulhU="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "nix-on-droid.cachix.org-1:56snoMJTXmDRC1Ei24CmKoUqvHJ9XCp+nidK7qkMQrU="
      "pre-commit-hooks.cachix.org-1:Pkk3Panw5AW24TOv6kz3PvLhlH8puAsJTBbOPmBo7Rc="
      "cache.lix.systems:aBnZUw8zA7H35Cz2RyKFVs3H4PlGTLawyY5KRbvJR8o="
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

    truecolor-check = {
      url = "git+https://gist.github.com/fdeaf79e921c2f413f44b6f613f6ad53.git";
      flake = false;
    };

    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    lix = {
      url = "git+https://git@git.lix.systems/lix-project/lix?ref=refs/tags/2.90-beta.1";
      flake = false;
    };
    lix-module = {
      url = "git+https://git.lix.systems/lix-project/nixos-module";
      inputs.lix.follows = "lix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

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

    flake-utils.url = "github:numtide/flake-utils";

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
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
        pre-commit-hooks-nix.follows = "git-hooks";
      };
    };

    nix-fast-build = {
      url = "github:bbigras/nix-fast-build/impure";
      inputs = {
        flake-parts.follows = "flake-parts";
        nixpkgs.follows = "nixpkgs";
      };
    };

    nix-index-database = {
      url = "github:Mic92/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs = {
        flake-compat.follows = "flake-compat";
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-hardware.url = "nixos-hardware";
    nur.url = "nur";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };

    # copilot_el = {
    #   url = "github:copilot-emacs/copilot.el";
    #   flake = false;
    # };

    copilot_pkgs.url = "github:bbigras/nixpkgs/copilot";
    copilot-node-server_pkgs.url = "github:bbigras/nixpkgs/copilot-node-server";

    envrc = {
      url = "github:siddharthverma314/envrc";
      flake = false;
    };

    defmacro-gensym = {
      url = "gitlab:akater/defmacro-gensym";
      flake = false;
    };

    org-tufte = {
      url = "github:Zilong-Li/org-tufte";
      flake = false;
    };

    nix-on-droid = {
      url = "github:t184256/nix-on-droid";
      inputs.home-manager.follows = "home-manager";
    };

    catppuccin.url = "github:catppuccin/nix";

    systems.url = "github:nix-systems/default";

    treefmt = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; }
      (toplevel@{ withSystem, ... }: {
        imports = [
          inputs.git-hooks.flakeModule
          inputs.treefmt.flakeModule
        ];
        systems = [ "aarch64-linux" "x86_64-linux" ];
        perSystem = ctx@{ config, self', inputs', pkgs, system, ... }: {
          _module.args.pkgs = import inputs.nixpkgs {
            localSystem = system;
            overlays = [ self.overlays.default ];
            config = {
              allowUnfree = true;
              allowAliases = true;
            };
          };

          devShells = import ./nix/dev-shell.nix ctx;

          packages = import ./nix/packages.nix toplevel ctx;

          pre-commit = {
            check.enable = true;
            settings.hooks = {
              actionlint.enable = true;
              # luacheck.enable = true;
              nil.enable = true;
              shellcheck = {
                enable = true;
                excludes = [
                  "users/bbigras/core/p10k-config/p10k.zsh"
                ];
              };
              statix.enable = false;
              # stylua.enable = true;
              treefmt.enable = true;
            };
          };

          treefmt = {
            projectRootFile = "flake.nix";
            programs = {
              nixpkgs-fmt.enable = true;
              shfmt = {
                enable = true;
                indent_size = 0;
              };
            };
            settings.formatter.nixpkgs-fmt.excludes = [ "hardware-configuration-*.nix" ];
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
      });
}
