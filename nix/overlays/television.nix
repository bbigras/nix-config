final: prev: {
  television =
    final.callPackage "${prev.__inputs.nixpkgs_television}/pkgs/by-name/te/television/package.nix"
      { };
}
