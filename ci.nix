let
  outputs = builtins.getFlake (toString ./.);
  pkgs = outputs.inputs.nixpkgs;
  drvs = pkgs.lib.collect pkgs.lib.isDerivation outputs.deploy;
in
drvs
