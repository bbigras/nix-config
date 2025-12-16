{
  flake,
  lib,
  pkgs,
  ...
}:
let
  inherit (flake) self;
in
{
  imports = with self.homeModules; [
    graphical-chromium
    # graphical-common
    graphical-firefox
    graphical-keepassxc
    graphical-psd
    graphical-vicinae
    graphical-zed-editor
    graphical-mime
    graphical-mpv
  ];

  dconf.enable = lib.mkForce true;

  home = {
    packages = with pkgs; [
      # adwaita-icon-theme
      # evince
      # gammastep
      # hicolor-icon-theme
      pavucontrol
      # pinentry-gnome3
      # qgnomeplatform
      # qt5.qtwayland
      # qt6.qtwayland
      # spawn
    ];

    sessionVariables = {
      MOZ_DBUS_REMOTE = 1;
      MOZ_USE_XINPUT2 = 1;
      QT_AUTO_SCREEN_SCALE_FACTOR = 1;
      _JAVA_OPTIONS = "-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dsun.java2d.xrender=true";
    };
  };

  gtk = {
    enable = true;
    gtk2.extraConfig = "gtk-application-prefer-dark-theme = true";
    gtk3.extraConfig.gtk-application-prefer-dark-theme = true;
  };

  qt = {
    enable = true;
    style.name = "kvantum";
  };

  services = {
    gpg-agent.pinentry.package = pkgs.pinentry-qt;
  };
}
