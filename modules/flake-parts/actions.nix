# CI workflow definition using actions.nix
{
  inputs,
  self,
  lib,
  ...
}:
let
  # Platform mappings: Nix system -> GitHub runner
  platforms = {
    x86_64-linux.os = "ubuntu-24.04";
    aarch64-linux.os = "ubuntu-22.04-arm";
  };

  # Build matrix entry from configuration (autodiscovery)
  # Returns data in the exact shape needed for GitHub Actions matrix
  mkHostInfo =
    kind: name: cfg:
    let
      platform = cfg.pkgs.stdenv.hostPlatform.system;
      platformInfo = platforms.${platform} or null;
    in
    lib.optionalAttrs (platformInfo != null) {
      inherit name;
      hostPlatform = platform;
      runsOn = platformInfo.os;
      attr =
        if kind == "nixos" then
          "nixosConfigurations.${name}.config.system.build.toplevel"
        else
          "homeConfigurations.${name}.activationPackage";
    };

  # Autodiscover all hosts and filter out unsupported platforms
  nixosHosts = lib.filter (h: h != { }) (
    lib.mapAttrsToList (mkHostInfo "nixos") (self.nixosConfigurations or { })
  );
  homeHosts = lib.filter (h: h != { }) (
    lib.mapAttrsToList (mkHostInfo "home") (self.homeConfigurations or { })
  );

  # GitHub Actions references - all versions consolidated here for Renovate
  actions = {
    alls-green = "re-actors/alls-green@05ac9388f0aebcb5727afa17fcccfecd6f8ec5fe"; # v1.2.2
    automerge = "peter-evans/enable-pull-request-automerge@a660677d5469627102a1c1e11409dd063606628d"; # v3.0.0
    cachix = "cachix/cachix-action@5f2d7c5294214f71b873db4b969586b980625e71"; # v17
    checkout = "actions/checkout@9c091bb21b7c1c1d1991bb908d89e4e9dddfe3e0"; # v7
    hestia = "Mic92/hestia@fb239a2f72d4b6e26eec5425f289dea23b27a527"; # v2
    nothing-but-nix = "wimpysworld/nothing-but-nix@687c797a730352432950c707ab493fcc951818d7"; # v10
    install-nix-action = "cachix/install-nix-action@630ae543ea3a38a9a4166f03376c02c50f408342"; # v31.11.0
  };

  # Reusable step definitions
  steps = {
    nothing-but-nix = {
      uses = actions.nothing-but-nix;
    };

    checkout = {
      uses = actions.checkout;
      "with".persist-credentials = false;
    };

    nixInstaller = {
      uses = actions.install-nix-action;
    };

    hestia = {
      uses = actions.hestia;
      "with" = {
        upstream-cache-filter = true;
        upstream-cache-key-names = "cache.nixos.org-1 nix-community.cachix.org-1";
      };
    };

    cachix = {
      uses = actions.cachix;
      "with" = {
        name = "bbigras-nix-config";
        authToken = "\${{ secrets.CACHIX_AUTH_TOKEN }}";
        extraPullNames = "nix-community,noctalia,niri";
      };
    };

    # Helper to create nix-fast-build step for a given attribute expression
    nix-fast-build = flakeAttr: {
      name = "nix-fast-build";
      run = "nix run '${flakeRef}#nix-fast-build' -- --no-nom --skip-cached --retries=3 --option accept-flake-config true --flake='${flakeRef}#${flakeAttr}'";
    };
  };

  # Common setup steps for build jobs
  setupSteps = [
    steps.nothing-but-nix
    steps.checkout
    steps.nixInstaller
    steps.hestia
    steps.cachix
  ];

  # Platforms to run flake check/show on (derived from all hosts)
  checkPlatforms =
    let
      allHosts = nixosHosts ++ homeHosts;
      hostPlatforms = lib.unique (map (h: h.hostPlatform) allHosts);
    in
    map (p: {
      platform = p;
      inherit (platforms.${p}) os;
    }) hostPlatforms;

  flakeRef = "git+file:.";
in
{
  imports = [ inputs.actions-nix.flakeModules.default ];

  flake.actions-nix = {
    pre-commit.enable = true;

    defaultValues.jobs = {
      timeout-minutes = 60;
      runs-on = "ubuntu-24.04";
    };

    workflows = {
      ".github/workflows/ci.yaml" = {
        name = "ci";

        on = {
          push.branches = [
            "master"
            "try"
          ];
          pull_request = { };
          workflow_dispatch = { };
        };

        concurrency = {
          group = "ci-\${{ github.head_ref || github.ref_name }}";
          cancel-in-progress = "\${{ github.event_name == 'pull_request' }}";
        };

        # Minimal permissions for security - this workflow only needs to read code
        permissions = { };

        jobs = {
          # Flake check on all platforms
          flake-check = {
            name = "flake check (\${{ matrix.systems.platform }})";
            environment = "ci";
            strategy.matrix.systems = checkPlatforms;
            runs-on = "\${{ matrix.systems.os }}";
            steps = setupSteps ++ [
              {
                name = "nix flake check";
                run = "nix flake check '${flakeRef}'";
              }
              {
                name = "nix flake show";
                run = "nix flake show '${flakeRef}'";
              }
            ];
          };

          # Build hosts directly (NixOS + home-manager on any platform)
          build = {
            name = "\${{ matrix.attrs.name }} (\${{ matrix.attrs.hostPlatform }})";
            environment = "ci";
            strategy = {
              fail-fast = false;
              matrix.attrs = nixosHosts ++ homeHosts;
            };
            runs-on = "\${{ matrix.attrs.runsOn }}";
            steps = setupSteps ++ [ (steps.nix-fast-build "\${{ matrix.attrs.attr }}") ];
          };

          # Final check job - aggregates all results
          check = {
            runs-on = "ubuntu-24.04";
            needs = [
              "flake-check"
              "build"
            ];
            "if" = "always()";
            steps = [
              {
                uses = actions.alls-green;
                "with" = {
                  jobs = "\${{ toJSON(needs) }}";
                };
              }
            ];
          };
        };
      };

      # Regenerate workflows for Renovate PRs or manual trigger
      ".github/workflows/regenerate-workflows.yaml" = {
        name = "regenerate-workflows";

        on = {
          pull_request.paths = [
            "modules/flake-parts/actions.nix"
            "flake.lock"
          ];
          workflow_dispatch = { };
        };

        permissions = {
          contents = "write";
          pull-requests = "write";
        };

        jobs.regenerate = {
          runs-on = "ubuntu-24.04";
          environment = "renovate";
          # Only run for Renovate PRs or manual dispatch
          "if" = "github.actor == 'renovate[bot]' || github.event_name == 'workflow_dispatch'";
          steps = [
            (
              steps.checkout
              // {
                "with" = {
                  ref = "\${{ github.head_ref || github.ref_name }}";
                  token = "\${{ secrets.PAT }}";
                  fetch-depth = 2;
                };
              }
            )
            steps.nixInstaller
            steps.hestia
            steps.cachix
            {
              name = "Regenerate workflows";
              run = "nix run .#render-workflows";
            }
            {
              name = "Amend commit with regenerated workflows";
              run = ''
                git config user.name "github[bot]"
                git config user.email "noreply@github.com"
                git add .github/workflows/
                git diff --staged --quiet || git commit --amend --no-edit
                git push --force-with-lease
              '';
            }
            {
              uses = actions.automerge;
              "with" = {
                token = "\${{ secrets.PAT }}";
                pull-request-number = "\${{ github.event.pull_request.number }}";
                merge-method = "rebase";
              };
            }
          ];
        };
      };
    };
  };
}
