{ lib, pkgs, ... }:

{
  programs.btop = {
    enable = true;
    catppuccin.enable = true;
  };
}
