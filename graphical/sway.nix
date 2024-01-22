{ pkgs, ... }:

{
  programs.sway.enable = true;
  programs.zsh.loginShellInit = ''
    if [[ "$(tty)" == /dev/tty1 ]]; then
      exec sway &> /dev/null
    fi
  '';

  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];
    wlr.enable = true;
  };
}
