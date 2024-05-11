{ pkgs, ... }:

{
  programs.dconf.enable = true;

  services = {
    dbus.packages = with pkgs; [ dconf ];
    xserver.enable = true;
    xserver.desktopManager.gnome.enable = true;
    xserver.displayManager.gdm = {
      enable = true;
      autoSuspend = true;
      wayland = false;
    };
    accounts-daemon.enable = true;
    gnome = {
      # at-spi2-core.enable = true;
      gnome-keyring.enable = true;
      gnome-settings-daemon.enable = true;
    };
  };
}
