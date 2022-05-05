{ lib, pkgs, ... }: {
  imports = [
    ./alacritty.nix
    ./common.nix
    ./firefox.nix
    ./mpv.nix
    ./redshift.nix
    ./mime.nix
  ];

  home = {
    packages = with pkgs; [
      blueman
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
      speedcrunch
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

    sessionVariables = {
      MOZ_DBUS_REMOTE = 1;
      MOZ_USE_XINPUT2 = 1;
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
    blueman-applet.enable = true;
    gpg-agent.pinentryFlavor = "gnome3";
    gammastep = {
      enable = true;
      provider = "geoclue2";
      tray = true;
      settings.general = {
        brightness-day = 1.0;
        brightness-night = 0.4;
      };
    };
  };

  systemd.user.services = {
    gammastep.Unit = {
      After = [ "geoclue-agent.service" ];
      Wants = [ "geoclue-agent.service" ];
    };
    polkit = {
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

  home.pointerCursor = {
    package = pkgs.gnome.adwaita-icon-theme;
    name = "Adwaita";
  };
}
