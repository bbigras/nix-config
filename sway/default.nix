{ pkgs, ... }: {
  imports = [
    ./boot-silent.nix
    ./fonts.nix
    ./location.nix
    ./sway.nix
  ];

  environment.systemPackages = with pkgs; [
    adwaita-qt
    gnome3.adwaita-icon-theme
    hicolor-icon-theme
    qgnomeplatform
    qt5.qtwayland
    # mon
  ];

  qt5 = {
    enable = true;
    platformTheme = "gnome";
    style = "adwaita";
  };

  nixpkgs.overlays = [
    (import ../overlays/spawn.nix)
    (import ../overlays/screenocr.nix)
    (import ../overlays/menu)
  ];

  xdg = {
    autostart.enable = false;
    icons.enable = true;
    menus.enable = true;
    mime.enable = true;
    portal = {
      enable = true;
      extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];
    };
  };
}
