{ pkgs, ... }: {
  # silent boot for plymouth
  boot = {
    consoleLogLevel = 0;
    kernelParams = [
      "quiet"
      "udev.log_level=3"
    ];
  };

  programs.dconf.enable = true;

  services = {
    dbus.packages = with pkgs; [ dconf ];
    gnome.at-spi2-core.enable = true;
    xserver.enable = true;
    xserver.displayManager.gdm = {
      enable = true;
      autoSuspend = true;
      wayland = true;
    };
  };

  xdg.portal = {
    enable = true;
    wlr.enable = true;
    extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];
  };
}
