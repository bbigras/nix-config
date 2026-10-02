{
  programs.noctalia = {
    enable = true;
    settings = {

      shell = {
        # font = "JetBrainsMono Nerd Font";
        settings_show_advanced = true;
      };

      theme = {
        mode = "dark";
        source = "builtin";
        builtin = "Catppuccin";
      };

      bar = {
        density = "compact";
        position = "right";
        showCapsule = false;
        widgets = {
          left = [
            {
              id = "ControlCenter";
              useDistroLogo = true;
            }
            {
              id = "Network";
            }
            {
              id = "Bluetooth";
            }
          ];
          center = [
            {
              hideUnoccupied = false;
              id = "Workspace";
              labelMode = "none";
            }
          ];
          right = [
            {
              alwaysShowPercentage = false;
              id = "Battery";
              warningThreshold = 30;
            }
            {
              formatHorizontal = "HH:mm";
              formatVertical = "HH mm";
              id = "Clock";
              useMonospacedFont = true;
              usePrimaryColor = true;
            }
          ];
        };
      };
      location = {
        # monthBeforeDay = true;
        name = "Montreal, Qubec";
      };
    };
  };

  # from https://docs.noctalia.dev/noctalia/compositor-settings/niri/
  wayland.windowManager.niri = {
    settings = {
      spawn-at-startup = "noctalia";

      window-rule = {
        # Rounded corners for a modern look.
        geometry-corner-radius = 20;

        # Clips window contents to the rounded corner boundaries.
        clip-to-geometry = true;
      };

      debug = {
        # Allows notification actions and window activation from Noctalia.
        honor-xdg-activation-with-invalid-serial = { };
      };

      binds = {
        # hotkey-overlay-title="Open a Terminal: alacritty" { spawn "alacritty"; }
        "Mod+T" = {
          _props.hotkey-overlay-title = "Open a Terminal: ghostty";
          spawn-sh = "ghostty";
        };

        # Core Noctalia binds
        "Mod+Space" = {
          spawn-sh = "noctalia msg panel-toggle launcher";
        };
        "Mod+S" = {
          spawn-sh = "noctalia msg panel-toggle control-center";
        };
        "Mod+Comma" = {
          spawn-sh = "noctalia msg settings-toggle";
        };
        "Alt+Tab" = {
          spawn-sh = "noctalia msg window-switcher hold";
        };
        # Niri has a built-in window switcher you might want to try it to see which one you prefer.

        # Audio & Brightness
        XF86AudioRaiseVolume = {
          spawn-sh = "noctalia msg volume-up";
        };
        XF86AudioLowerVolume = {
          spawn-sh = "noctalia msg volume-down";
        };
        XF86AudioMute = {
          spawn-sh = "noctalia msg volume-mute";
        };
        XF86MonBrightnessUp = {
          spawn-sh = "noctalia msg brightness-up";
        };
        XF86MonBrightnessDown = {
          spawn-sh = "noctalia msg brightness-down";
        };

        XF86AudioPlay = {
          spawn-sh = "noctalia msg media toggle";
        };
        # XF86AudioStop = {
        #   spawn-sh = "noctalia msg media stop";
        # };
        XF86AudioPrev = {
          spawn-sh = "noctalia msg media previous";
        };
        XF86AudioNext = {
          spawn-sh = "noctalia msg media next";
        };
      };

    };
    # The pinned Home Manager KDL generator does not render root-level
    # `_children`, so emit the second repeated top-level node directly.
    extraConfig = ''
      window-rule {
        match app-id="dev.noctalia.Noctalia"
        open-floating true
        default-column-width {
          fixed 1080
        }
        default-window-height {
          fixed 920
        }
      }
    '';
  };
}
