{ pkgs, ... }:

{
  home.packages = with pkgs; [
    qt6.qtwebsockets
    grim
  ];

  programs.noctalia = {
    enable = true;
  };
}
