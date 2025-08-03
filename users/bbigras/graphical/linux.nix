{ lib, pkgs, ... }:
{
  imports = [
    ./bongocat.nix
    ./firefox.nix
    ./mpv.nix
    ./tkey-ssh-agent.nix
    ./zed-editor.nix
  ];

  home = {
    packages = with pkgs; [
      pinentry-gnome3
    ];

    sessionVariables = {
      MOZ_DBUS_REMOTE = 1;
      MOZ_USE_XINPUT2 = 1;
      PAGER = "less -FRX";
      QT_AUTO_SCREEN_SCALE_FACTOR = 1;
      _JAVA_OPTIONS = "-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dsun.java2d.xrender=true";
    };
  };

  catppuccin = {
    cursors.enable = true;
    gtk = {
      icon.enable = true;
    };
    kvantum.enable = true;
  };

  gtk = {
    enable = true;
    gtk2.extraConfig = "gtk-application-prefer-dark-theme = true";
    gtk3.extraConfig.gtk-application-prefer-dark-theme = true;
    gtk4.extraConfig.gtk-application-prefer-dark-theme = 1;
  };

  qt = {
    enable = true;
    platformTheme.name = "kvantum";
    style.name = "kvantum";
  };

  services = {
    # gammastep = {
    #   enable = false;
    #   provider = "geoclue2";
    #   tray = true;
    #   settings.general = {
    #     brightness-day = lib.mkDefault 1.0;
    #     brightness-night = lib.mkDefault 0.4;
    #   };
    # };
    # udiskie = {
    #   enable = true;
    #   automount = false;
    #   tray = "auto";
    # };
    gpg-agent.pinentry.package = pkgs.pinentry-qt;
  };
}
