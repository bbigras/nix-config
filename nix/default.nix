let
  sources = import ./sources.nix;
in
{
  inherit (sources) nixpkgs nixus nix-matrix-yggdrasil NUR nixpkgs-mozilla emacs-overlay neuron;
  home-manager = sources.home-manager + "/nixos";
  lib = sources.nixpkgs + "/lib";
  cpu_intel = sources.nixos-hardware + "/common/cpu/intel";
  ssd = sources.nixos-hardware + "/common/pc/ssd";
  xps-13-9343 = sources.nixos-hardware + "/dell/xps/13-9343";
}
