{ lib, pkgs, ... }: {
  imports = [
    ./alacritty.nix
    ./common.nix
    ./firefox.nix
    # ./mpv.nix
    # ./redshift.nix
  ];

  home = {
    # file.".icons/default".source = "${pkgs.gnome3.adwaita-icon-theme}/share/icons/Adwaita";

    packages = with pkgs; [
      libnotify
      # lollypop
      pavucontrol
      pinentry-gnome
      # speedcrunch
    ] ++ lib.optionals (pkgs.hostPlatform.system == "x86_64-linux") [
      discord
      element-desktop
      # gnome.evince
      # gnome.gnome-calendar
      imv
      # mbk
      # prusa-slicer
      shotwell
      # signal-desktop
      # slack
      # spotify
      # thunderbird
      # zoom-us
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

  services.gpg-agent.pinentryFlavor = "gnome3";

  systemd.user.services = {
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
    # evolution-alarm-notify = {
    #   Unit = {
    #     Description = "evolution-alarm-notify";
    #     PartOf = [ "graphical-session.target" ];
    #   };
    #   Service = {
    #     ExecStart = "${pkgs.gnome.evolution-data-server}/libexec/evolution-data-server/evolution-alarm-notify";
    #     RestartSec = 3;
    #     Restart = "always";
    #   };
    #   Install = {
    #     WantedBy = [ "graphical-session.target" ];
    #   };
    # };
  };

  xsession.pointerCursor = {
    package = pkgs.gnome3.adwaita-icon-theme;
    name = "Adwaita";
  };
}
