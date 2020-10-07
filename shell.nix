let
  pkgs = import (import ./nix).nixpkgs { };
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    cachix
    niv
    nixpkgs-fmt

    # keep this line if you use bash
    pkgs.bashInteractive
  ];

  shellHook = "${(import ./.).preCommitChecks.shellHook}";
}
