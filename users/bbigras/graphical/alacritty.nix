{ pkgs, ... }:
let
  inherit (pkgs.stdenv) hostPlatform;
in
{
  programs.alacritty = {
    enable = true;
  };
}
