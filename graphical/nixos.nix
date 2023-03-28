{ pkgs, ... }: {
  boot = {
    consoleLogLevel = 3;
    kernelParams = [
      "quiet"
      "systemd.show_status=auto"
      "udev.log_level=3"
      "vt.global_cursor_default=0"
    ];
  };

  # programs.dconf.enable = true;

  # security.pam.services.gdm-fingerprint.text = ''
  #   account  include    login

  #   auth     requisite  pam_nologin.so
  #   auth     required   pam_succeed_if.so uid >= 1000 quiet
  #   auth     sufficient ${pkgs.fprintd}/lib/security/pam_fprintd.so
  #   auth     optional   ${pkgs.gnome.gnome-keyring}/lib/security/pam_gnome_keyring.so

  #   password required   ${pkgs.fprintd}/lib/security/pam_fprintd.so
  #   password optional   ${pkgs.gnome.gnome-keyring}/lib/security/pam_gnome_keyring.so use_authok

  #   session  include    login
  # '';

  # services = {
  #   dbus.packages = with pkgs; [ dconf ];
  #   gnome.at-spi2-core.enable = true;
  #   xserver.enable = true;
  #   xserver.displayManager.gdm = {
  #     enable = true;
  #     autoSuspend = true;
  #     # wayland = true;
  #   };
  # };

  # xdg.portal = {
  #   enable = true;
  #   wlr.enable = true;
  #   extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];
  # };
}
