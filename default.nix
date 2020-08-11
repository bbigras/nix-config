{ pkgs ? null }:
let
  sources = import ./nix;
  nixus = import sources.nixus { };
  nixos-hardware = import sources.nixos-hardware;
  nixpkgs = if pkgs == null then sources.nixpkgs else pkgs;
in
nixus ({ ... }: {
  defaults = { ... }: { inherit nixpkgs; };
  nodes = {
    # Personal
    desktop = { ... }: {
      enabled = true;
      host = "desktop";
      configuration = ./systems/desktop.nix;
    };

    laptop = { ... }: {
      enabled = true;
      host = "laptop";
      configuration = ./systems/laptop.nix;
    };

    # work
    work = { ... }: {
      enabled = false;
      hasFastConnection = true;
      host = "work";
      configuration = ./systems/work.nix;
    };
  };
})
