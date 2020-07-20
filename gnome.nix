{ pkgs, ... }: {
  imports = [];

  environment.systemPackages = with pkgs; [
    adwaita-qt
    gnome3.adwaita-icon-theme
    hicolor-icon-theme
    qgnomeplatform
    qt5.qtwayland
    # mon

    # gnome3.gnome-shell-extension-appindicator-32
    gnomeExtensions.appindicator
  ];

  qt5 = {
    enable = false;
    platformTheme = "gnome";
    style = "adwaita";
  };

  # nixpkgs.overlays = [];

  # xdg = {
  #   autostart.enable = false;
  #   icons.enable = true;
  #   menus.enable = true;
  #   mime.enable = true;
  #   portal = {
  #     enable = true;
  #     extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];
  #   };
  # };

  services.xserver.displayManager.gdm.enable = true;
  services.xserver.displayManager.gdm.wayland = false;
  services.xserver.desktopManager.gnome3.enable = true;

  # Running ancient applications
  services.dbus.packages = with pkgs; [ gnome2.GConf ];

  # Systray Icons
  # environment.systemPackages = [ gnome-shell-extension-appindicator-32 ];
  services.udev.packages = with pkgs; [ gnome3.gnome-settings-daemon ];

}
