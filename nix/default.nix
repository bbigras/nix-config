let
  sources = import ./sources.nix;
in
rec {
  inherit (sources) nixpkgs nix-matrix-yggdrasil NUR nixpkgs-mozilla emacs-overlay impermanence crate2nix emacs-pgtk-nativecomp-overlay;
  home-manager = sources.home-manager + "/nixos";
  lib = import (nixpkgs + "/lib");
  gitignoreSource = (import sources.gitignore { inherit lib; }).gitignoreSource;
  cpu_intel = sources.nixos-hardware + "/common/cpu/intel";
  ssd = sources.nixos-hardware + "/common/pc/ssd";
  xps-13-9343 = sources.nixos-hardware + "/dell/xps/13-9343";
  nix-pre-commit-hooks = (import (sources.nix-pre-commit-hooks + "/nix") { inherit nixpkgs; }).packages;
  nixus = import sources.nixus { };
}
