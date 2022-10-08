{ lib, pkgs, ... }: {
  imports = [
    ./alacritty.nix
    ./common.nix
    ./firefox.nix
    ./kitty.nix
    ./mpv.nix
    ./mime.nix
  ];

  home = {
    packages = with pkgs; [
      gammastep
      gnome.adwaita-icon-theme
      hicolor-icon-theme
      libnotify
      lollypop
      pavucontrol
      pinentry-gnome
      qgnomeplatform
      qt5.qtwayland
      spawn
      qalculate-gtk
      xdg-utils
    ] ++ lib.optionals (pkgs.hostPlatform.system == "x86_64-linux") [
      discord
      element-desktop
      gnome.evince
      gnome.gnome-calendar
      mbk
      obsidian
      shotwell
      signal-desktop
      thunderbird
      zoom-us
    ];

    pointerCursor = {
      package = pkgs.gnome.adwaita-icon-theme;
      name = "Adwaita";
      gtk.enable = true;
    };

    sessionVariables = {
      MOZ_DBUS_REMOTE = 1;
      MOZ_USE_XINPUT2 = 1;
      QT_AUTO_SCREEN_SCALE_FACTOR = 1;
      _JAVA_OPTIONS = "-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dsun.java2d.xrender=true";
    };
  };

  gtk = {
    enable = true;
    font.name = "sans";
    gtk2.extraConfig = "gtk-application-prefer-dark-theme = true";
    gtk3.extraConfig.gtk-application-prefer-dark-theme = true;
    theme = {
      package = pkgs.ayu-theme-gtk;
      name = "Ayu-Dark";
    };
  };

  qt = {
    enable = true;
    platformTheme = "gnome";
    style = {
      name = "adwaita";
      package = pkgs.adwaita-qt;
    };
  };

  services = {
    gpg-agent.pinentryFlavor = "gnome3";
    gammastep = {
      enable = false;
      provider = "geoclue2";
      tray = true;
      settings.general = {
        brightness-day = 1.0;
        brightness-night = 0.4;
      };
    };
    udiskie = {
      enable = true;
      automount = false;
      tray = "auto";
    };
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
