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
      autoSuspend = false;
      wayland = false;
    };
    xserver.desktopManager.gnome.enable = true;
  };

  stylix = {
    cursor = {
      name = "Adwaita";
      package = pkgs.gnome.adwaita-icon-theme;
    };
    targets.plymouth.enable = false;
    targets.gnome.enable = true;
  };
}
