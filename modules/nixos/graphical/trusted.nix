{
  lib,
  pkgs,
  ...
}:
{
  programs = {
  };

  security.pam.services.login.enableGnomeKeyring = false;

  services = {
    dbus.packages = with pkgs; [ gcr_3 ];
    gnome.gnome-keyring.enable = lib.mkForce false;
  };
}
