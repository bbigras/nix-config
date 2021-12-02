{
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.displayManager.gdm.nvidiaWayland = false;
  services.xserver.displayManager.gdm.wayland = false;

  services.xserver.desktopManager.gnome.enable = true;

  programs.dconf.enable = true;
  services = {
    accounts-daemon.enable = true;
    gnome = {
      evolution-data-server.enable = true;
      gnome-keyring.enable = true;
      gnome-online-accounts.enable = true;
      gnome-online-miners.enable = true;
    };
  };
}
