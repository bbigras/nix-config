{
  # silent boot for plymouth
  boot = {
    consoleLogLevel = 0;
    kernelParams = [
      "quiet"
      "udev.log_level=3"
    ];
  };

  # programs.dconf.enable = true;
  programs.wireshark.enable = true;

  services = {
    # dbus.packages = with pkgs; [ dconf ];
    # displayManager.gdm = {
    #   enable = true;
    #   autoSuspend = true;
    #   wayland = true;
    # };
    # gnome.at-spi2-core.enable = true;
    orca.enable = false;
    speechd.enable = false;
    # xserver.enable = true;
  };

  # xdg.portal = {
  #   enable = true;
  #   wlr.enable = true;
  #   extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];
  # };
}
