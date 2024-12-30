{
  hostType,
  lib,
  pkgs,
  ...
}:
let
  fontPackages = with pkgs; [
    dejavu_fonts
    nerd-fonts.iosevka
    nerd-fonts.symbols-only
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-cjk-serif
    noto-fonts-color-emoji
    noto-fonts-extra
    unifont
  ];
in
{
  fonts = {
    packages = fontPackages;
    enableDefaultPackages = false;
    enableGhostscriptFonts = false;

    fontconfig = {
      defaultFonts = {
        monospace = [ "Iosevka Nerd Font" ];
      };
    };
  };
}
