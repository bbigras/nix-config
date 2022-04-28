{ pkgs, ... }:

{
  programs.sway.enable = true;

  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];
    gtkUsePortal = true;
    wlr.enable = true;
  };
}
