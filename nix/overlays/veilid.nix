self: super:
let
  pkgs = self.__inputs.nixpkgs_veilid.legacyPackages.${super.system};
in
{
  veilid = pkgs.veilid;
}
