{ pkgs, ... }:
{
  home.packages = with pkgs; [
    checkart
    fixart
    mediainfo
  ];
}
