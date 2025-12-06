{ pkgs, ... }:
{
  programs = {
  };

  security.pam.services.login.enableGnomeKeyring = false;

  services = {
    dbus.packages = with pkgs; [ gcr ];
    gnome.gnome-keyring.enable = false;
  };
}
