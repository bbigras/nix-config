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

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/af3adc21-df14-49b0-8d51-3b18f9dc8a82";
    fsType = "ext4";
  };

  boot.initrd.luks.devices."cryptroot".device =
    "/dev/disk/by-uuid/bfd7024b-39d6-4ba4-8517-967a8c2360c4";

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/339C-3957";
    fsType = "vfat";
    options = [
      "fmask=0022"
      "dmask=0022"
    ];
  };

  environment.systemPackages = with pkgs; [
    iwd
  ];

  programs.steam.enable = true;
  programs.steam.remotePlay.openFirewall = true;
  programs.gamemode.enable = true;

  sops.secrets = {
    restic-laptop-password.sopsFile = ./secrets.yaml;
    restic-laptop-creds.sopsFile = ./secrets.yaml;
    wireguard.sopsFile = ./secrets.yaml;
  };

  # boot.kernelPackages = pkgs.linuxPackages_latest;

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
  swapDevices = [
    {
      device = "/swapfile";
    }
  ];

  boot.initrd.availableKernelModules = [
    "aesni_intel"
    "cryptd"
  ];

  networking = {
    useNetworkd = true;
    hostName = "laptop"; # Define your hostname.
    wireless = {
      enable = false;
      iwd = {
        enable = true;
        settings = {
          Settings = {
            AutoConnect = true;
          };
        };
      };
    };
  };

  networking.networkmanager.enable = false;

  systemd.network = {
    enable = true;
    networks = {
      "10-wifi" = {
        DHCP = "yes";
        matchConfig.Name = "eth0";
        dhcpV4Config = {
          UseDNS = false;
        };
      };
    };
  };

  services.flatpak.enable = true;
  services.smartd.enable = true;
  services.gnome.gnome-keyring.enable = false;
  services.irqbalance.enable = true;

  services.earlyoom = {
    enable = true;
    enableNotifications = true;
  };
  services = {
    fwupd.enable = false;
    thermald.enable = true;
  };
  services.desktopManager.cosmic.enable = true;
  services.displayManager.cosmic-greeter.enable = true;
  environment.sessionVariables.COSMIC_DATA_CONTROL_ENABLED = 1;

  zramSwap = {
    enable = true;
    algorithm = "zstd";
  };

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
