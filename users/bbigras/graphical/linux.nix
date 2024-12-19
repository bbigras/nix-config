{ lib, pkgs, ... }: {
  imports = [
    ./firefox.nix
    ./mime.nix
    ./mpv.nix
  ];

  dconf.enable = lib.mkForce true;

  home = {
    packages = with pkgs; [
      evince
      gammastep
      hicolor-icon-theme
      pavucontrol
      pinentry-gnome3
      qgnomeplatform
      qt5.qtwayland
      qt6.qtwayland
      spawn
    ];

    sessionVariables = {
      DIRENV_LOG_FORMAT = "";
      MOZ_DBUS_REMOTE = 1;
      MOZ_USE_XINPUT2 = 1;
      PAGER = "less -FRX";
      QT_AUTO_SCREEN_SCALE_FACTOR = 1;
      _JAVA_OPTIONS = "-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dsun.java2d.xrender=true";
    };
  };

  gtk = {
    enable = true;
    gtk2.extraConfig = "gtk-application-prefer-dark-theme = true";
    gtk3.extraConfig.gtk-application-prefer-dark-theme = true;
    catppuccin = {
      enable = true;
      cursor.enable = true;
      gnomeShellTheme = true;
      icon.enable = true;
    };
  };

  qt = {
    enable = true;
    platformTheme.name = "kvantum";
    style = {
      name = "kvantum";
      catppuccin.enable = true;
    };
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
    gpg-agent.pinentryPackage = pkgs.pinentry-gnome3;
  };

  systemd.user.services = {
    polkit-gnome = {
      Unit = {
        Description = "polkit-gnome";
        Documentation = [ "man:polkit(8)" ];
        PartOf = [ "graphical-session.target" ];
      };
      Service = {
        Type = "simple";
        ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
        RestartSec = 3;
        Restart = "always";
      };
      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };
  };
}
