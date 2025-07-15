# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  config,
  pkgs,
  nur,
  ...
}:
let
  nurNoPkgs = import nur {
    pkgs = null;
    nurpkgs = pkgs;
  };
  json = pkgs.formats.json { };
in
{
  imports =
    [
      ../../core
      ../../dev
      # ../../dev/virt-manager.nix
      ../../services/kanata.nix
      ../../services/podman.nix
      ../../services/veilid.nix

      { config.facter.reportPath = ./facter.json; }

      # Include the results of the hardware scan.
      ../../hardware/efi.nix
      ../../hardware/sound.nix

      ../../graphical
      ../../graphical/trusted.nix

      # ../../dev/rust-embeded.nix
      ../../dev/adb.nix

      ../../users/bbigras
      ./disko.nix
    ]
    ++ (
      if builtins.pathExists (builtins.getEnv "PWD" + "/secrets/at_home.nix") then
        [ (builtins.getEnv "PWD" + "/secrets/at_home.nix") ]
      else
        [ ]
    )
    ++ (
      if builtins.pathExists (builtins.getEnv "PWD" + "/secrets/laptop.nix") then
        [ (builtins.getEnv "PWD" + "/secrets/laptop.nix") ]
      else
        [ ]
    );

  boot = {
    plymouth.enable = true;
  };

  environment.systemPackages = with pkgs; [
    iwd
    networkmanagerapplet
  ];

  programs.steam.enable = true;
  programs.steam.remotePlay.openFirewall = true;
  programs.gamemode.enable = true;

  sops.secrets = {
    networkmanager.sopsFile = ./secrets.yaml;
  };

  boot.kernelPackages = pkgs.linuxPackages_latest;

  catppuccin = {
    enable = true;
    flavor = "mocha";
    accent = "blue";
  };

  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [ ];

  hardware.brillo.enable = true;
  boot.kernel.sysctl = {
    "kernel.sysrq" = 1;
    # "fs.inotify.max_user_watches" = 524288;
    # "vm.swappiness" = 1;
  };

  networking = {
    hostName = "laptop"; # Define your hostname.
    wireless.enable = false;
  };

  networking.networkmanager = {
    enable = true;
    dns = "systemd-resolved";
    wifi = {
      backend = "iwd";
      macAddress = "stable-ssid";
      # powersave = false;
    };
    ensureProfiles = {
      environmentFiles = [
        config.sops.secrets.networkmanager.path
      ];
      profiles = {
        home = {
          connection = {
            id = "home";
            type = "wifi";
            # uuid = "1a2538e1-1a72-4162-bc85-58682ecd91eb";
            autoconnect = true;
          };
          ipv4 = {
            method = "auto";
          };
          ipv6 = {
            addr-gen-mode = "default";
            method = "auto";
          };
          # proxy = { };
          wifi = {
            ssid = "$HOME_SSID";
          };
          wifi-security = {
            key-mgmt = "wpa-psk";
            psk = "$HOME_PASSPHRASE";
          };
        };
        phone = {
          connection = {
            autoconnect-priority = "-1";
            id = "phone";
            metered = "1";
            # timestamp = "1752765639";
            type = "wifi";
            # uuid = "d3c51cff-ccaa-481e-8d93-a02c5ac3bdda";
          };
          ipv4 = {
            method = "auto";
          };
          ipv6 = {
            addr-gen-mode = "stable-privacy";
            method = "auto";
          };
          # proxy = { };
          wifi = {
            # cloned-mac-address = "stable-ssid";
            mode = "infrastructure";
            ssid = "$PHONE_PASSPHRASE";
          };
          wifi-security = {
            key-mgmt = "wpa-psk";
            psk = "$PHONE_PSK";
          };
        };
      };
    };
  };

  # These options are unnecessary when managing DNS ourselves
  networking.useDHCP = false;
  networking.dhcpcd.enable = false;

  services.flatpak.enable = true;
  services.smartd.enable = true;
  services.gnome.gnome-keyring.enable = false;

  services.earlyoom = {
    enable = true;
    enableNotifications = true;
  };
  services = {
    fwupd = {
      enable = true;
      extraRemotes = [ "lvfs-testing" ];
      # uefiCapsuleSettings.DisableCapsuleUpdateOnDisk = true;
    };
    thermald.enable = true;
  };
  services.desktopManager.cosmic.enable = true;
  services.displayManager.cosmic-greeter.enable = true;
  environment.sessionVariables.COSMIC_DATA_CONTROL_ENABLED = 1;

  home-manager.users.bbigras = {
    imports = [
      ../../users/bbigras/trusted
      nurNoPkgs.repos.rycee.hmModules.emacs-init
    ];

    services.wluma = {
      enable = true;
      settings = {
        als = {
          webcam = {
            video = 0;
            thresholds = {
              "0" = "night";
              "15" = "dark";
              "30" = "dim";
              "45" = "normal";
              "60" = "bright";
              "75" = "outdoors";
            };
          };
        };

        output.backlight = [
          {
            name = "eDP-1";
            path = "/sys/class/backlight/intel_backlight";
            # capturer = "wayland";
            capturer = "none";
          }
        ];

        keyboard = [
          {
            name = "keyboard-dell";
            path = "/sys/bus/platform/devices/dell-laptop/leds/dell::kbd_backlight";
          }
        ];
      };
    };

    xdg.configFile = {
      "easyeffects/autoload/output/alsa_output.pci-0000_00_1b.0.analog-stereo:analog-output-lineout.json".source =
        json.generate "alsa_output.pci-0000_00_1b.0.analog-stereo:analog-output-lineout.json" {
          "device" = "alsa_output.pci-0000_00_1b.0.analog-stereo";
          "device-description" = "Audio interne Stéréo analogique";
          "device-profile" = "analog-output-speaker";
          "preset-name" = "Perfect EQ";
        };
    };
  };
}
