{ pkgs, config, ... }: {
  imports = [ ];

  environment.systemPackages = with pkgs; [
    adwaita-qt
    gnome3.adwaita-icon-theme
    hicolor-icon-theme
    qgnomeplatform
    qt5.qtwayland
    # mon
  ];

  boot.extraModulePackages = [ config.boot.kernelPackages.v4l2loopback ];

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

  services = {
    xserver = {
      enable = true;
      layout = "ca";
      xkbVariant = "fr";
      displayManager.gdm.enable = true;
      displayManager.gdm.wayland = false;
      desktopManager.gnome3.enable = true;
    };

    dbus.packages = [ pkgs.gnome3.dconf ];
    udev.packages = [ pkgs.gnome3.gnome-settings-daemon ];

    # Running ancient applications
    # services.dbus.packages = with pkgs; [ gnome2.GConf ];
  };
}
