{ pkgs, ... }:

{
  programs.sway.enable = true;

  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];
    wlr.enable = true;
  };
}
