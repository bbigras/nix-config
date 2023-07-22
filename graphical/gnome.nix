{ pkgs, ... }:

{
  programs.dconf.enable = true;
  services = {
    accounts-daemon.enable = true;
    gnome = {
      at-spi2-core.enable = true;
      gnome-keyring.enable = true;
    };
    dbus.packages = with pkgs; [ dconf ];
    xserver.enable = true;
    xserver.displayManager.gdm = {
      enable = true;
      autoSuspend = true;
      wayland = false;
    };
    xserver.desktopManager.gnome.enable = true;
  };

  stylix.targets.plymouth.enable = false;
  stylix.targets.gnome.enable = true;
}
