{ hostType, lib, pkgs, ... }:
let
  fontPackages = with pkgs; [
    dejavu_fonts
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-cjk-serif
    noto-fonts-extra
    unifont
    (nerdfonts.override { fonts = [ "Iosevka" "NerdFontsSymbolsOnly" ]; })
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
