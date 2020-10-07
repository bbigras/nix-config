let
  sources = import ./nix;
in
{
  preCommitChecks = sources.nix-pre-commit-hooks.run {
    src = sources.gitignoreSource ./.;
    hooks = {
      nix-linter = {
        enable = true;
        excludes = [
          # "overlays/menu/default.nix"
        ];
      };
      nixpkgs-fmt.enable = true;
    };
    excludes = [
      "nix/sources.json"
      "nix/sources.nix"
      "hardware/hardware-configuration-work.nix"
      "hardware/hardware-configuration-laptop.nix"
      "hardware/hardware-configuration-desktop.nix"
      "users/bbigras/core/default.nix"
    ];
  };

  deploy = sources.nixus {
    defaults = { ... }: { nixpkgs = sources.nixpkgs; };
    nodes = {
      # Personal
      desktop = { ... }: {
        enabled = true;
        hasFastConnection = true;
        host = "desktop";
        configuration = ./systems/desktop.nix;
      };

      laptop = { ... }: {
        enabled = true;
        hasFastConnection = true;
        host = "laptop";
        configuration = ./systems/laptop.nix;
      };

      # work
      work = { ... }: {
        enabled = true;
        host = "work";
        configuration = ./systems/work.nix;
      };
    };
  };
}
