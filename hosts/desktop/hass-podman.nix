{ pkgs, ... }:

# pkgs.formats.yaml

let
  # hassConfig = (builtins.toJSON {}).generate "home-assistant-configuration" {
  hassConfig = {
    config = { };
    frontend = { };
    history = { };
    image = { };
    input_boolean = { };
    input_datetime = {
      good_morning = {
        name = "LED Alarm";
        has_time = true;
      };
    };
    input_number = { };
    input_select = { };
    logbook = { };
    mobile_app = { };
    prometheus = { };
    person = { };
    ssdp = { };
    sun = { };
    system_health = { };
    updater = { };
    zeroconf = { };
    zone = { };

    # automation = {};
    script = { };

    sensor = [
      {
        platform = "aftership";
        api_key = "196b6168-a8bf-4ad9-bda5-d5d8c1f979a2";
      }
      {
        platform = "template";
        sensors = {
          light_state = {
            friendly_name = "Overal lighting status";
            unit_of_measurement = "°";
            value_template = ''
              {{ (states.sun.sun.attributes.elevation | float - (states('sensor.weatherbit_cloud_coverage') | float / 30)) | round(1) }}
            '';
            icon_template = ''
              {% if (states.sun.sun.attributes.elevation | float - (states('sensor.weatherbit_cloud_coverage') | float / 30)) > 0 %}
                mdi:lightbulb
              {% else %}
                mdi:lightbulb-on-outline
              {% endif %}
            '';
          };
        };
      }
    ];

    weather = [
      { platform = "environment_canada"; }
    ];

    camera = [
      { platform = "environment_canada"; }
    ];

    # zwave = {
    #   usb_path = "/dev/ttyUSB0";
    # };

    zwave_js = { };

    zha = {
      usb_path = "/dev/ttyUSB1";
      database_path = "/config/zigbee.db";
    };

    spotify = {
      client_id = "7426f34051864d8ea77d06860b13c078";
      client_secret = "b3622df8e0e84ebe918d760e2a60dbb7";
    };

    # http = {
    #   use_x_forwarded_for = true;
    #   trusted_proxies = config.vpn.hosts.sosiego.vpnIPs;
    # };

    group = { };
    # automation = import ./hass/automations.nix {inherit lib;};
    # script = "!include scripts.yaml";
    # scene = { };
  };

  configFile = config: pkgs.runCommand "config.yaml"
    {
      buildInputs = [ pkgs.remarshal ];
      preferLocalBuild = true;
      allowSubstitutes = false;
    } ''
    remarshal -if json -of yaml \
      < ${pkgs.writeText "verbs.json" (builtins.toJSON config)} \
      > $out
      echo "automation: !include automations.yaml" >> $out
      echo "scene: !include scenes.yaml" >> $out
      #echo "script: !include scripts.yaml" >> $out
  '';

  # < ${pkgs.writeText "config.json" (builtins.toJSON config)} \

in
{
  # virtualisation.oci-containers.backend = "podman";
  virtualisation.oci-containers.containers.home-assistant = {
    image = "docker.io/homeassistant/home-assistant";
    volumes = [
      "/var/lib/hass:/config"
      "/etc/localtime:/etc/localtime:ro"
      "${configFile hassConfig}:/config/configuration.yaml:ro"
    ];
    # devices = [
    #   "/dev/ttyUSB0:/dev/ttyUSB0"
    #   "/dev/ttyUSB1:/dev/ttyUSB1"
    # ];
    extraOptions = [
      "--net=host"
      "--pull=always"
      # "--device=/dev/ttyUSB0" # z-wave
      "--device=/dev/ttyUSB1" # zigbee
    ];
    dependsOn = [
      "zwavejs2mqtt"
    ];
  };

  environment.persistence."/persist" = {
    directories = [
      "/var/lib/hass"
      "/var/lib/zwavejs2mqtt"
    ];
  };

  services.restic.backups = {
    desktop = {
      paths = [
        "/var/lib/hass"
        "/var/lib/zwavejs2mqtt"
      ];
    };
  };

  virtualisation.oci-containers.containers.zwavejs2mqtt = {
    image = "zwavejs/zwavejs2mqtt";
    ports = [
      "8091:8091" # port for web interface
      "3009:3009" # port for Z-Wave JS websocket server
    ];
    extraOptions = [
      "--net=host"
      # "--device=/dev/ttyUSB0"
      "--device=/dev/serial/by-id/usb-Silicon_Labs_HubZ_Smart_Home_Controller_515014F3-if00-port0:/dev/zwave"
    ];
    volumes = [
      "/var/lib/zwavejs2mqtt:/usr/src/app/store"
      "/etc/localtime:/etc/localtime:ro"
    ];
    environment = {
      TZ = "America/Montreal";
    };
  };

  systemd.services.podman-home-assistant.serviceConfig.RestartSec = "10s";
  # Hack to get podman working with zfs
  # systemd.services.podman-home-assistant.path = [ config.boot.zfs.package ];
}
