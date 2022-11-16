{ pkgs, ... }: {
  programs = {
    seahorse.enable = true;
  };

  security.pam.services.login.enableGnomeKeyring = true;

  services = {
    dbus.packages = with pkgs; [ gcr ];
    gnome.gnome-keyring.enable = true;
  };
}
