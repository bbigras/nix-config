{ config, lib, pkgs, ... }: {
  wayland.windowManager.sway = {
    enable = true;
    config = {
      bars = [ ];

      input = {
        "type:keyboard".xkb_options = "caps:escape";
      };

      keybindings =
        let
          inherit (config.wayland.windowManager.sway.config) modifier terminal;
        in
        lib.mkOptionDefault {
          "${modifier}+Return" = "exec ${pkgs.foot}/bin/foot";
          "${modifier}+Shift+q" = "kill";
          "${modifier}+d" = "exec ${pkgs.dmenu}/bin/dmenu_path | ${pkgs.dmenu}/bin/dmenu | ${pkgs.findutils}/bin/xargs swaymsg exec --";
        };

      terminal = lib.getExe pkgs.foot;

      window.commands = [
        { command = "floating enable"; criteria.app_id = "imv"; }
      ];
    };

    extraConfig = ''
      include /etc/sway/config.d/*
    '';

    # extraOptions = [ "--unsupported-gpu" ];

    extraSessionCommands = ''
      export LIBSEAT_BACKEND="logind"

      export ECORE_EVAS_ENGINE=wayland_egl
      export ELM_ENGINE=wayland_egl
      export MOZ_ENABLE_WAYLAND=1
      export QT_QPA_PLATFORM=wayland
      export SDL_VIDEODRIVER=wayland
      export NIXOS_OZONE_WL=1

      export MOZ_DBUS_REMOTE=1
      export MOZ_USE_XINPUT2=1

      export QT_AUTO_SCREEN_SCALE_FACTOR=1
      export QT_WAYLAND_DISABLE_WINDOWDECORATION=1
      export QT_WAYLAND_FORCE_DPI=physical

      export _JAVA_AWT_WM_NONREPARENTING=1
      export _JAVA_OPTIONS="-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dsun.java2d.xrender=true"

      export WLR_DRM_FORCE_LIBLIFTOFF=1
      export WLR_RENDERER=vulkan
    '';

    systemdIntegration = true;
    wrapperFeatures.gtk = true;
  };
}
