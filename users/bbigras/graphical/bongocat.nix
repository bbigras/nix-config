{
  pkgs,
  config,
  ...
}:

let
  settingsFormat = pkgs.formats.ini { };

  configDir =
    if (pkgs.stdenv.targetPlatform.isDarwin) then
      "Library/Application Support/bongocat"
    else
      "${config.xdg.configHome}/bongocat";

  settings = {
    main = {
      cat_height = 50; # Size of bongo cat (16-128)
      cat_x_offset = 100; # Horizontal position offset
      cat_y_offset = 0; # Vertical position offset
      enable_debug = 0; # Show debug messages
      fps = 60; # Frame rate (1-120)
      keyboard_device = "/dev/input/event24"; # kanata
      overlay_opacity = 0;
    };
  };
  configFile = "${configDir}/settings.conf";
in
{
  home = {
    packages = [ pkgs.wayland-bongocat ];
    file.bongocat-settings = {
      target = "${configDir}/settings.conf";
      source = settingsFormat.generate "bongocat-settings" settings;
    };
  };
  systemd.user.services = {
    bongocat = {
      Unit = {
        After = [ "graphical-session.target" ];
        PartOf = [ "graphical-session.target" ];
      };
      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
      Service = {
        Restart = "on-failure";
        PrivateTmp = true;
        ProtectSystem = "full";
        Type = "exec";
        Slice = "session.slice"; # ?
        ExecStart = "${pkgs.wayland-bongocat}/bin/bongocat --config ${configFile}";
      };
    };
  };
}
