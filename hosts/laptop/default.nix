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
      ../../dev/virt-manager.nix

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

  # https://www.reddit.com/r/NixOS/comments/p8bqvu/how_to_install_v4l2looback_kernel_module/
  boot.kernelPackages = pkgs.linuxPackages_latest;
  # boot.kernelModules = [ "kvm-intel" "hid-nintendo" "v4l2loopback" ];
  # boot.kernelModules = [ "v4l2loopback" ];
  # boot.extraModulePackages = [ config.boot.kernelPackages.v4l2loopback ];

  hardware.uinput.enable = true;

  catppuccin = {
    enable = true;
    flavor = "mocha";
    accent = "blue";
  };

  # boot.kernelModules = [ "uinput" ];
  boot = {
    extraModulePackages = with config.boot.kernelPackages; [
      # v4l2loopback.out
    ];
    kernelModules = [
      # "v4l2loopback"
    ];
    # extraModprobeConfig = ''
    # options v4l2loopback exclisive_caps=1 card_label="Virtual Camera"
    # '';
  };

  hardware.opengl = { enable = true; extraPackages = with pkgs; [ libva vaapiVdpau libvdpau-va-gl ]; };
  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [ 445 ];

  # boot.extraModulePackages = [
  # config.boot.kernelPackages.v4l2loopback.out
  # ];
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

  systemd.network.wait-online.enable = false;
  boot.initrd.systemd.network.wait-online.enable = false;
  networking.wireless.dbusControlled = false;

  services.flatpak.enable = true;
  services.smartd.enable = true;

  # systemd.user.services.mpris-proxy = {
  #   description = "Mpris proxy";
  #   after = [ "network.target" "sound.target" ];
  #   script = "${pkgs.bluez}/bin/mpris-proxy";
  #   wantedBy = [ "default.target" ];
  # };

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
    # fwupd.enable = true;
    tlp = {
      enable = true;
      settings = {
        #CPU_SCALING_GOVERNOR_ON_AC = "ondemand";
        #CPU_SCALING_GOVERNOR_ON_BAT = "conservative";

        #PLATFORM_PROFILE_ON_AC = "performance";
        #PLATFORM_PROFILE_ON_BAT = "low-power";

        DEVICES_TO_DISABLE_ON_BAT_NOT_IN_USE = [ "bluetooth" "wifi" ];
        DEVICES_TO_ENABLE_ON_AC = [ "bluetooth" "wifi" ];

        DISK_IOSCHED = [ "none" ];

        START_CHARGE_THRESH_BAT0 = 70;
        STOP_CHARGE_THRESH_BAT0 = 80;
      };
    };
  };

  systemd.targets = {
    ac = {
      description = "On AC power";
      unitConfig.DefaultDependencies = false;
    };
    battery = {
      description = "On battery power";
      unitConfig.DefaultDependencies = false;
    };
  };

  services = {
    udev.extraRules = ''
      SUBSYSTEM=="power_supply", KERNEL=="AC", ATTR{online}=="0", RUN+="${config.systemd.package}/bin/systemctl start battery.target"
      SUBSYSTEM=="power_supply", KERNEL=="AC", ATTR{online}=="0", RUN+="${config.systemd.package}/bin/systemctl stop ac.target"
      SUBSYSTEM=="power_supply", KERNEL=="AC", ATTR{online}=="1", RUN+="${config.systemd.package}/bin/systemctl start ac.target"
      SUBSYSTEM=="power_supply", KERNEL=="AC", ATTR{online}=="1", RUN+="${config.systemd.package}/bin/systemctl stop battery.target"

      SUBSYSTEM=="power_supply", KERNEL=="AC", ATTR{online}=="0", RUN+="${config.systemd.package}/bin/systemctl --user --machine=bbigras@.host start battery.target"
      SUBSYSTEM=="power_supply", KERNEL=="AC", ATTR{online}=="0", RUN+="${config.systemd.package}/bin/systemctl --user --machine=bbigras@.host stop ac.target"
      SUBSYSTEM=="power_supply", KERNEL=="AC", ATTR{online}=="1", RUN+="${config.systemd.package}/bin/systemctl --user --machine=bbigras@.host start ac.target"
      SUBSYSTEM=="power_supply", KERNEL=="AC", ATTR{online}=="1", RUN+="${config.systemd.package}/bin/systemctl --user --machine=bbigras@.host stop battery.target"
      SUBSYSTEM="usb", ATTR{idVendor}=="2833", ATTR{idProduct}=="0186", MODE="0660" group="plugdev", symlink+="ocuquest%n"
    '';
  };

  systemd.timers = {
    logrotate.partOf = [ "ac.target" ];
    logrotate.wantedBy = [ "ac.target" ];
    systemd-tmpfiles-clean.partOf = [ "ac.target" ];
    systemd-tmpfiles-clean.wantedBy = [ "ac.target" ];
    fstrim.partOf = [ "ac.target" ];
    fstrim.wantedBy = [ "ac.target" ];
  };

  home-manager.users.bbigras = {
    imports = [
      ../../users/bbigras/trusted
    ];

    systemd.user.targets = {
      ac = {
        Unit = {
          Description = "On AC power";
        };
      };
      battery = {
        Unit = {
          Description = "On battery power";
        };
      };
    };

    systemd.user.services = {
      brightness-ac = {
        Unit = {
          Description = "100% brightness on ac";
          PartOf = [ "ac.target" ];
        };
        Service = {
          Type = "oneshot";
          ExecStart = "${pkgs.brillo}/bin/brillo -e -S 100";
        };
        Install = {
          WantedBy = [ "ac.target" ];
        };
      };

      brightness-battery = {
        Unit = {
          Description = "lower brightness on battery";
          PartOf = [ "battery.target" ];
        };
        Service = {
          Type = "oneshot";
          ExecStart = "${pkgs.brillo}/bin/brillo -e -S 50";
        };
        Install = {
          WantedBy = [ "battery.target" ];
        };
      };
    };

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
