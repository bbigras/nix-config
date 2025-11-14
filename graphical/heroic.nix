{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    (heroic.override {
      extraPkgs = pkgs: [
        pkgs.gamescope

        pkgs.gdk-pixbuf
        pkgs.gtk3
        pkgs.adwaita-icon-theme
      ];
    })
  ];
}
