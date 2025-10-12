{
  pkgs,
  system,
  winboat,
  ...
}:

{
  environment.systemPackages = [
    winboat.packages.${system}.winboat
    pkgs.freerdp
  ];
}
