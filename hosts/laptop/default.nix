# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, nixos-hardware, ... }:

let
in
{
  imports = with nixos-hardware.nixosModules;
    [
      ../../core
      ../../dev
      ../../dev/incus.nix
      # ../../dev/virt-manager.nix
      ../../services/veilid.nix

      # Include the results of the hardware scan.
      ../../hardware/hardware-configuration-laptop.nix
      ../../hardware/efi.nix
      ../../hardware/bluetooth.nix
      ../../hardware/sound.nix
      dell-xps-13-9343
      common-hidpi

      ../../graphical
      ../../graphical/sway.nix
      ../../graphical/trusted.nix

      # ../../dev/rust-embeded.nix
      ../../dev/adb.nix

      ../../users/bbigras
    ] ++ (if builtins.pathExists (builtins.getEnv "PWD" + "/secrets/at_home.nix") then [ (builtins.getEnv "PWD" + "/secrets/at_home.nix") ] else [ ])
    ++ (if builtins.pathExists (builtins.getEnv "PWD" + "/secrets/laptop.nix") then [ (builtins.getEnv "PWD" + "/secrets/laptop.nix") ] else [ ]);

  environment.systemPackages = with pkgs; [
    iwd
  ];

  services.avahi.enable = true;

  sops.secrets = {
    restic-laptop-password.sopsFile = ./restic-laptop.yaml;
    restic-laptop-creds.sopsFile = ./restic-laptop.yaml;
    wireguard.sopsFile = ./restic-laptop.yaml;
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
    wireless.iwd = {
      enable = true;
      settings = {
        Settings = {
          AutoConnect = true;
        };
      };
    };
  };

  systemd.network = {
    enable = true;
    networks = {
      "10-wifi" = {
        DHCP = "yes";
        matchConfig.Name = "wlan*";
        dhcpV4Config = { UseDNS = false; };
      };
    };
  };

  services.flatpak.enable = true;
  services.smartd.enable = true;

  services.earlyoom = {
    enable = true;
    enableNotifications = true;
    extraArgs =
      let
        catPatterns = patterns: builtins.concatStringsSep "|" patterns;
        preferPatterns = [
          ".firefox-wrappe"
          "hercules-ci-age"
          "ipfs"
          "java" # If it's written in java it's uninmportant enough it's ok to kill it
          ".jupyterhub-wra"
          "Logseq"
          "rust-analyzer"
        ];
        avoidPatterns = [
          "bash"
          "mosh-server"
          "sshd"
          "systemd"
          "systemd-logind"
          "systemd-udevd"
          "tmux: client"
          "tmux: server"
        ];
      in
      [
        "--prefer '^(${catPatterns preferPatterns})$'"
        "--avoid '^(${catPatterns avoidPatterns})$'"
      ];
  };
  services = {
    fwupd.enable = true;
    thermald.enable = true;
    tlp = {
      enable = true;
      settings = {
        CPU_SCALING_GOVERNOR_ON_AC = "performance";
        CPU_SCALING_GOVERNOR_ON_BAT = "powersave";

        CPU_ENERGY_PERF_POLICY_ON_BAT = "power";
        CPU_ENERGY_PERF_POLICY_ON_AC = "performance";

        CPU_MIN_PERF_ON_AC = 0;
        CPU_MAX_PERF_ON_AC = 100;
        CPU_MIN_PERF_ON_BAT = 0;
        CPU_MAX_PERF_ON_BAT = 20;

        #Optional helps save long term battery health
        START_CHARGE_THRESH_BAT0 = 40; # 40 and bellow it starts to charge
        STOP_CHARGE_THRESH_BAT0 = 80; # 80 and above it stops charging
      };
    };
  };

  zramSwap = {
    enable = true;
    algorithm = "zstd";
  };

  home-manager.users.bbigras = {
    imports = [
      ../../users/bbigras/trusted
    ];

    services.gnome-keyring = {
      enable = true;
      components = [ "secrets" ];
    };

    wayland.windowManager.sway = {
      config = {
        input = {
          "1:1:AT_Translated_Set_2_keyboard" = {
            repeat_rate = "70";
            xkb_layout = "ca";
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
        };

        output = {
          "*" = {
            scale = "2";
            # bg = "~/Downloads/molly.png fit";
          };
        };
      };
    };
  };
}
