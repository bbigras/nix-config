{ hostType, lib, pkgs, ... }:
let
  fontPackages = with pkgs; [
    dejavu_fonts
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-cjk-serif
    noto-fonts-extra
    unifont
    (nerdfonts.override { fonts = [ "Iosevka" ]; })
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

  stylix.fonts = {
    sansSerif = {
      package = pkgs.ibm-plex;
      name = "IBM Plex Sans";
    };
    serif = {
      package = pkgs.ibm-plex;
      name = "IBM Plex Serif";
    };
    monospace = {
      package = pkgs.nerdfonts.override { fonts = [ "Hack" ]; };
      name = "Hack Nerd Font";
    };
    emoji = {
      package = pkgs.noto-fonts-emoji;
      name = "Noto Color Emoji";
    };
  };
}
