{ pkgs, ... }:
let
  my_waybar = pkgs.waybar.override { pulseSupport = true; withMediaPlayer = true; };

  config = {
    layer = "top";
    modules-left = [ "sway/workspaces" "sway/mode" "custom/media" ];
    modules-right = [
      "pulseaudio"
      "idle_inhibitor"
      "network"
      # "cpu"
      # "memory"
      "temperature"
      "backlight"
      "battery"
      "clock"
      "tray"
    ];
    "sway/mode" = { format = ''<span style="italic">{}</span>''; };
    "sway/workspaces" = {
      all-outputs = true;
      format = "{name}";
    };
    idle_inhibitor = {
      format = "{icon}";
      format-icons = {
        activated = "";
        deactivated = "";
      };
    };
    tray = {
      icon-size = 20;
      spacing = 5;
    };
    clock = {
      tooltip-format = "{calendar}";
      format = "{:%F | %H:%M}";
    };
    cpu = {
      format = "{usage}% ";
      tooltip = false;
    };
    memory = { format = "{}% "; };
    temperature = {
      thermal-zone = 1;
      critical-threshold = 80;
      format = "{temperatureC}°C {icon}";
      format-icons = [ "" "" "" ];
    };
    backlight = {
      device = "intel_backlight";
      format = "{percent}% {icon}";
      format-icons = [ "" "" ];
      on-scroll-up = "${pkgs.brillo}/bin/brillo -e -A 0.5";
      on-scroll-down = "${pkgs.brillo}/bin/brillo -e -U 0.5";
    };
    battery = {
      bat = "BAT0";
      states = {
        good = 90;
        warning = 30;
        critical = 15;
      };
      format = "{capacity}% {icon}";
      format-charging = "{capacity}% ";
      format-plugged = "{capacity}% ";
      format-alt = "{time} {icon}";
      format-icons = [ "" "" "" "" "" ];
    };
    network = {
      format-wifi = "{essid} ({signalStrength}%) ";
      format-ethernet = "{ifname}: {ipaddr}/{cidr} ";
      format-linked = "{ifname} (No IP) ";
      format-disconnected = "Disconnected ⚠";
      format-alt = "{ifname}: {ipaddr}/{cidr}";
    };
    pulseaudio = {
      format = "{volume}% {icon} {format_source}";
      format-bluetooth = "{volume}% {icon} {format_source}";
      format-bluetooth-muted = " {icon} {format_source}";
      format-muted = " {format_source}";
      format-source = "{volume}% ";
      format-source-muted = "";
      format-icons = {
        headphones = "";
        handsfree = "";
        headset = "";
        phone = "";
        portable = "";
        car = "";
        default = [ "" "" "" ];
      };
      on-click = "${pkgs.ponymix}/bin/ponymix -t sink toggle";
      on-scroll-up = "${pkgs.ponymix}/bin/ponymix increase 1";
      on-scroll-down = "${pkgs.ponymix}/bin/ponymix decrease 1";
    };

    "custom/media" = {
      format = "{icon} {}";
      return-type = "json";
      max-length = 40;
      format-icons = {
        spotifyd = "";
        default = "🎜";
      };
      escape = true;
      exec = "${my_waybar}/bin/waybar-mediaplayer.py 2> /dev/null";
    };

  };
in
{
  xdg.configFile.waybar = {
    target = "waybar/config";
    text = (builtins.toJSON config);
  };

  xdg.configFile.waybar-style = {
    target = "waybar/style.css";
    text = ''
* {
    border: none;
    border-radius: 0;
    font-family: Roboto, Helvetica, Arial, sans-serif;
    font-size: 13px;
    min-height: 0;
}

window#waybar {
    background: rgba(43, 48, 59, 0.5);
    border-bottom: 3px solid rgba(100, 114, 125, 0.5);
    color: white;
}

#workspaces button {
    padding: 0 5px;
    background: transparent;
    color: white;
    border-bottom: 3px solid transparent;
}

#workspaces button.focused {
    background: #64727D;
    border-bottom: 3px solid white;
}

#mode {
    background: #64727D;
    border-bottom: 3px solid white;
}

#clock, #battery, #cpu, #memory, #network, #pulseaudio, #custom-spotify, #tray, #mode, #custom-weather, #custom-storage, #idle_inhibitor {
    padding: 0 10px;
    margin: 0 5px;
}

#clock {
    background-color: #64727D;
}

#battery {
    background-color: #ffffff;
    color: black;
}

#battery.charging {
    color: white;
    background-color: #26A65B;
}

@keyframes blink {
    to {
        background-color: #ffffff;
        color: black;
    }
}

#battery.warning:not(.charging) {
    background: #f53c3c;
    color: white;
    animation-name: blink;
    animation-duration: 0.5s;
    animation-timing-function: linear;
    animation-iteration-count: infinite;
    animation-direction: alternate;
}

#cpu {
    background: #2ecc71;
    color: #000000;
}

#memory {
    background: #9b59b6;
}

#network {
    background: #2980b9;
}

#network.disconnected {
    background: #f53c3c;
}

#pulseaudio {
    background: #f1c40f;
    color: black;
}

#pulseaudio.muted {
    background: #90b1b1;
    color: #2a5c45;
}

#custom-spotify {
    background: #66cc99;
    color: #2a5c45;
}

#tray {
    background-color: #2980b9;
}

#custom-storage.warning {
    color:      rgba(255, 210, 4, 1);
}

#custom-storage.critical {
    color:      rgba(238, 46, 36, 1);
}
    '';
  };
}
