final: prev: {
  wayland-bongocat =
    final.callPackage "${prev.__inputs.nixpkgs_bongocat}/pkgs/by-name/wa/wayland-bongocat/package.nix"
      { };
}
