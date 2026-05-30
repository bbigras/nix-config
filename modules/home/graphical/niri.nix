{
  config,
  lib,
  pkgs,
  ...
}:

let
  apps = {
    browser = "firefox";
    terminal = "${lib.getExe pkgs.ghostty}";
    fileManager = "thunar";
    screenshotArea = "${pkgs.bash}/bin/bash -c '${pkgs.grim}/bin/grim -g \"\\\$(${pkgs.slurp}/bin/slurp)\" - | ${pkgs.wl-clipboard}/bin/wl-copy'";
    screenshotWindow = "${pkgs.bash}/bin/bash -c '${pkgs.grim}/bin/grim -g \"\\\$(${pkgs.slurp}/bin/slurp -w)\" - | ${pkgs.wl-clipboard}/bin/wl-copy'";
    screenshotOutput = "${pkgs.bash}/bin/bash -c '${pkgs.grim}/bin/grim - | ${pkgs.wl-clipboard}/bin/wl-copy'";
  };

  noctalia =
    cmd:
    [
      "noctalia"
      "ipc"
      "call"
    ]
    ++ (pkgs.lib.splitString " " cmd);
in
{
  services.gnome-keyring.enable = lib.mkForce false;

  home.packages = [
    pkgs.noctalia-qs
    pkgs.thunar
    pkgs.xwayland-satellite
  ];

  programs.niri = {
    enable = true;
    package = pkgs.niri;
    settings = {
      prefer-no-csd = true;
      hotkey-overlay.skip-at-startup = true;
      input.keyboard.xkb = {
        layout = "ca";
        variant = "fr";
      };

      spawn-at-startup = [
        {
          argv = [
            "noctalia"
          ];
        }
      ];

      binds = with config.lib.niri.actions; {
        "Mod+O".action = toggle-overview;
        "Mod+Comma".action = consume-window-into-column;
        "Mod+Period".action = expel-window-from-column;
        "Mod+R".action = switch-preset-column-width;
        "Mod+Shift+R".action = switch-preset-column-width-back;
        "Mod+Ctrl+Shift+R".action = switch-preset-window-height;
        "Mod+Ctrl+R".action = reset-window-height;
        "Mod+F".action = maximize-column;
        "Mod+Shift+F".action = fullscreen-window;
        "Mod+M".action = maximize-window-to-edges;
        "Mod+Ctrl+F".action = expand-column-to-available-width;
        "Mod+C".action = center-column;
        "Mod+Ctrl+C".action = center-visible-columns;
        # "Mod+Minus".action = set-column-width "-10%";
        # "Mod+Equal".action = set-column-width "+10%";
        "Mod+W".action = toggle-column-tabbed-display;

        "super+F1".action.spawn = noctalia "plugin:keybind-cheatsheet toggle";
        # Volume
        "XF86AudioRaiseVolume".action.spawn = noctalia "volume increase"; # output increase
        "XF86AudioLowerVolume".action.spawn = noctalia "volume decrease"; # output decrease
        "XF86AudioMute".action.spawn = noctalia "volume muteOutput"; # output mute
        "shift+XF86AudioRaiseVolume".action.spawn = noctalia "volume increaseInput"; # input increase
        "shift+XF86AudioLowerVolume".action.spawn = noctalia "volume decreaseInput"; # input decrease
        "shift+XF86AudioMute".action.spawn = noctalia "volume muteInput"; # input mute
        "control+XF86AudioMute".action.spawn = noctalia "volume togglePanel"; # open volume panel

        # Media
        "XF86AudioPlay".action.spawn = noctalia "media playPause";
        "XF86AudioNext".action.spawn = noctalia "media next";
        "XF86AudioPrev".action.spawn = noctalia "media previous";

        "super+Space".action = spawn [
          "${lib.getExe pkgs.vicinae}"
          "toggle"
        ];
        "super+q".action = close-window;
        "super+b".action = spawn apps.browser;
        "super+Return".action = spawn apps.terminal;
        #    "super+Space".action = spawn apps.appLauncher;
        "super+E".action = spawn apps.fileManager;
        "super+L".action.spawn = noctalia "lockScreen lock";

        "super+f".action = fullscreen-window;
        "super+t".action = toggle-window-floating;

        "control+shift+1".action.screenshot = [ ];
        "control+shift+2".action.screenshot-window = [ ];

        "super+Left".action = focus-column-left;
        "super+Right".action = focus-column-right;
        "super+Down".action = focus-workspace-down;
        "super+Up".action = focus-workspace-up;

        "super+Shift+Left".action = move-column-left;
        "super+Shift+Right".action = move-column-right;
        "super+Shift+Down".action = move-column-to-workspace-down;
        "super+Shift+Up".action = move-column-to-workspace-up;

        "super+1".action = focus-workspace "main";
        "super+2".action = focus-workspace "browser";
        "super+3".action = focus-workspace "discord";
        "super+4".action = focus-workspace "music";
      };

      layer-rules = [
        {
          # Set the overview wallpaper on the backdrop
          matches = [
            {
              namespace = "^noctalia-wallpaper*";
            }
          ];
          place-within-backdrop = true;
        }
      ];

      window-rules = [
        {
          matches = [
            {
              app-id = "org.keepassxc.KeePassXC";
            }
            {
              app-id = "firefox";
              title = "Incrustation vidéo";
            }
            {
              app-id = "steam";
              title = "Friends List";
            }
            {
              app-id = "firefox";
              title = "Page Info — *";
            }
            {
              app-id = "firefox";
              title = "Extension : (Gestionnaire de mots de passe Bitwarden)";
            }
            {
              app-id = "firefox";
              title = "Extension : (Kagi Search for Firefox)";
            }
            {
              app-id = "org.remmina.Remmina";
              title = "Visionneur de bureaux distants Remmina";
            }
          ];
          open-floating = true;
        }

        {
          matches = [ { } ];
          geometry-corner-radius = {
            top-left = 20.0;
            top-right = 20.0;
            bottom-left = 20.0;
            bottom-right = 20.0;
          };
          clip-to-geometry = true;
        }
      ];

      environment = {
        CLUTTER_BACKEND = "wayland";
        GDK_BACKEND = "wayland,x11";
        MOZ_ENABLE_WAYLAND = "1";
        NIXOS_OZONE_WL = "1";
        QT_QPA_PLATFORM = "wayland";
        QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
        ELECTRON_OZONE_PLATFORM_HINT = "auto";

        XDG_SESSION_TYPE = "wayland";
        XDG_CURRENT_DESKTOP = "niri";
      };
    };
  };
}
