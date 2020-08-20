{ lib, pkgs, ... }: {
  wayland.windowManager.sway = {
    enable = true;
    config = rec {
      bars = [ ];
      colors = {
        focused = {
          border = "#30535F";
          background = "#30535F";
          text = "#F0BC8D";
          childBorder = "#A43C0F";
          indicator = "#A43C0F";
        };
        unfocused = {
          border = "#00122A";
          background = "#00122A";
          text = "#F0BC8D";
          childBorder = "#A43C0F";
          indicator = "#A43C0F";
        };
        urgent = {
          border = "#A43C0F";
          background = "#A43C0F";
          text = "#000000";
          childBorder = "#A43C0F";
          indicator = "#A43C0F";
        };
      };

      floating = {
        border = 0;
        modifier = modifier;
      };

      focus.followMouse = false;

      fonts = [ "FontAwesome 8" "Iosevka 8" ];

      gaps = {
        inner = 10;
        outer = 5;
        smartBorders = "on";
      };

      input = {
        "1:1:AT_Translated_Set_2_keyboard" = {
          xkb_layout = "ca";
          # xkb_numlock = "enabled";
          repeat_rate = "70";
        };
        "1739:30381:DLL0665:01_06CB:76AD_Touchpad" = {
          # dwt = "enabled";
          # tap = "enabled";
          # natural_scroll = "enabled";
          # middle_emulation = "enabled";

          accel_profile = "adaptive";
          click_method = "button_areas";
          dwt = "disabled";
          natural_scroll = "enabled";
          scroll_method = "two_finger";
          tap = "enabled";
        };

        # "2:7:SynPS/2_Synaptics_TouchPad" = {
        #   accel_profile = "adaptive";
        #   click_method = "button_areas";
        #   dwt = "disabled";
        #   natural_scroll = "enabled";
        #   scroll_method = "two_finger";
        #   tap = "enabled";
        # };

      };

      keybindings = lib.mkOptionDefault {
        "${modifier}+Return" = "exec ${terminal}";
        "${modifier}+d" = "exec ${menu}";
        "${modifier}+m" = "exec ${pkgs.emojimenu}/bin/emojimenu";
        "${modifier}+o" = "exec ${pkgs.otpmenu}/bin/otpmenu";
        "${modifier}+p" = "exec ${pkgs.passmenu}/bin/passmenu";
        "${modifier}+q" = "exec ${pkgs.swaylock}/bin/swaylock -f";
        "Mod1+Tab" = " workspace next";
        "Mod4+Tab" = " workspace prev";
        "Mod4+comma" = " workspace prev";
        "Mod4+period" = " workspace next";
        # "Print" = "exec ${pkgs.prtsc}/bin/prtsc";
        "Print" = "exec ${pkgs.grim}/bin/grim -g \"$(${pkgs.slurp}/bin/slurp)\" - | ${pkgs.wl-clipboard}/bin/wl-copy";
        "Mod1+Print" = "exec ${pkgs.grim}/bin/grim -o $(swaymsg -t get_outputs | jq -r '.[] | select(.focused) | .name') $(xdg-user-dir PICTURES)/$(date +'%Y-%m-%d-%H%M%S_grim.png')";
        "XF86AudioLowerVolume" = "exec ${pkgs.ponymix}/bin/ponymix decrease 1";
        "XF86AudioMicMute" = "exec ${pkgs.ponymix}/bin/ponymix -t source toggle";
        "XF86AudioMute" = "exec ${pkgs.ponymix}/bin/ponymix -t sink toggle";
        "XF86AudioNext" = "exec ${pkgs.playerctl}/bin/playerctl next";
        "XF86AudioPause" = "exec ${pkgs.playerctl}/bin/playerctl pause";
        "XF86AudioPlay" = "exec ${pkgs.playerctl}/bin/playerctl play";
        "XF86AudioPrev" = "exec ${pkgs.playerctl}/bin/playerctl previous";
        "XF86AudioRaiseVolume" = "exec ${pkgs.ponymix}/bin/ponymix increase 1";
        "XF86MonBrightnessDown" = "exec ${pkgs.brillo}/bin/brillo -e -U 10";
        "XF86MonBrightnessUp" = "exec ${pkgs.brillo}/bin/brillo -e -A 10";
      };

      menu = "${terminal} -d 55 18 -t swaymenu -e ${pkgs.swaymenu}/bin/swaymenu";

      modifier = "Mod4";

      output = {
        "*" = {
          scale = "2";
          # bg = "~/Downloads/molly.png fit";
        };
        # "*" = { bg = "~/.wall fill"; };
        # "Unknown 0x32EB 0x00000000" = {
        #   position = "0,0";
        #   mode = "3840x2160@60Hz";
        #   scale = "2";
        #   subpixel = "rgb";
        # };
        # "Goldstar Company Ltd LG Ultra HD 0x00000B08" = {
        #   position = "1920,0";
        #   mode = "3840x2160@60Hz";
        #   scale = "2";
        #   subpixel = "rgb";
        # };
        # "Dell Inc. DELL U2518D 0WG2J7C4A2AL" = {
        #   position = "1920,0";
        #   mode = "3840x2160@60Hz";
        #   scale = "2";
        #   subpixel = "rgb";
        #   transform = "90";
        # };
      };

      startup = [ ];

      terminal = "${pkgs.alacritty}/bin/alacritty";

      window = {
        border = 0;
        commands =
          let
            makeMenuWindow = "floating enable, border pixel 5, sticky enable";
          in
          [
            { command = makeMenuWindow; criteria = { app_id = "Alacritty"; title = "swaymenu"; }; }
            { command = makeMenuWindow; criteria = { app_id = "Alacritty"; title = "gopassmenu"; }; }
            { command = makeMenuWindow; criteria = { app_id = "Alacritty"; title = "emojimenu"; }; }
            { command = "floating enable"; criteria.app_id = "imv"; }
            { command = "floating enable, sticky enable"; criteria = { app_id = "firefox"; title = "Picture-in-Picture"; }; }
          ];
      };
    };

    extraSessionCommands = ''
      export ECORE_EVAS_ENGINE=wayland_egl
      export ELM_ENGINE=wayland_egl
      export MOZ_ENABLE_WAYLAND=1
      export QT_QPA_PLATFORM=wayland
      export QT_WAYLAND_DISABLE_WINDOWDECORATION=1
      export QT_WAYLAND_FORCE_DPI=physical
      export SDL_VIDEODRIVER=wayland
      export _JAVA_AWT_WM_NONREPARENTING=1
      export _JAVA_OPTIONS="-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dsun.java2d.xrender=true"
    '';

    systemdIntegration = true;
    wrapperFeatures.gtk = true;
  };
}
