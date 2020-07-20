{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    cachix
    niv
    nixpkgs-fmt

    # keep this line if you use bash
    pkgs.bashInteractive
  ];
}
