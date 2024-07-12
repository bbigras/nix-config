final: prev:
{
  incus = final.__inputs.nixpkgs_incus.legacyPackages.${final.stdenv.hostPlatform.system}.incus;
}
