# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, rycee-nur-expressions, nixos-hardware, ... }:

let
  qemu-aarch64-static = pkgs.stdenv.mkDerivation {
    name = "qemu-aarch64-static";

    src = builtins.fetchurl {
      url = "https://github.com/multiarch/qemu-user-static/releases/download/v6.1.0-6/qemu-aarch64-static";
      sha256 = "0c41s036iazrxn2ff86cvry51kj4fc0ygi7pway4z42ax0f46vm4";
    };

    dontUnpack = true;
    installPhase = "install -D -m 0755 $src $out/bin/qemu-aarch64-static";
  };

  rycee-nur-expressions2 = import rycee-nur-expressions { inherit pkgs; };
in
rec {
  imports = with nixos-hardware.nixosModules;
    [
      ../../core
      ../../veilid
      ../../dev
      ../../dev/virt-manager.nix

      # Include the results of the hardware scan.
      ../../hardware/hardware-configuration-desktop.nix
      ../../hardware/efi.nix
      ../../hardware/secureboot.nix

      common-pc
      common-pc-ssd
      common-cpu-intel-cpu-only
      common-gpu-nvidia-nonprime

      ../../hardware/sound.nix

      # ./aarch64.nix

      ../../graphical
      ../../graphical/gnome.nix
      ../../graphical/trusted.nix

      # ../../dev/rust-embeded.nix
      ../../dev/adb.nix

      ../../users/bbigras
      # ./hass-podman.nix
    ] ++ (if builtins.pathExists (builtins.getEnv "PWD" + "/secrets/at_home.nix") then [ (builtins.getEnv "PWD" + "/secrets/at_home.nix") ] else [ ])
    ++ (if builtins.pathExists (builtins.getEnv "PWD" + "/secrets/desktop.nix") then [ (builtins.getEnv "PWD" + "/secrets/desktop.nix") ] else [ ]);

  fileSystems = {
    "/".options = [ "compress=zstd" ];
    "/home".options = [ "compress=zstd" ];
    "/nix".options = [ "compress=zstd" "noatime" ];
    "/persist" =
      {
        options = [ "compress=zstd" "noatime" ];
        neededForBoot = true;
      };
    #"/swap".options = [ "noatime" ];
  };

  nix = {
    extraOptions = ''
      extra-platforms = aarch64-linux i686-linux
      trusted-users = bbigras
    '';
    settings = {
      extra-sandbox-paths = [ "/run/binfmt/aarch64=${qemu-aarch64-static}/bin/qemu-aarch64-static" ];
      system-features = [ "benchmark" "nixos-test" "big-parallel" "kvm" ];
    };
  };

  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.legacy_470;
  boot = {
    binfmt.registrations.aarch64 = {
      interpreter = "${qemu-aarch64-static}/bin/qemu-aarch64-static";
      magicOrExtension = ''\x7fELF\x02\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\xb7\x00'';
      mask = ''\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\x00\xff\xfe\xff\xff\xff'';
    };
    kernelPackages = pkgs.linuxPackages_zen;
    loader.grub.useOSProber = true;

    loader.systemd-boot.memtest86.enable = true;

    kernel.sysctl = {
      "kernel.sysrq" = 1;
      # "fs.inotify.max_user_watches" = 524288;
      # "vm.swappiness" = 1;
    };
  };

  home-manager.users.bbigras = {
    imports = [
      ../../users/bbigras/trusted
      rycee-nur-expressions2.hmModules.emacs-init
    ];

    xdg.mimeApps.enable = lib.mkForce false;
  };

  sops.secrets = {
    restic-desktop-password.sopsFile = ./restic-desktop.yaml;
    restic-desktop-creds.sopsFile = ./restic-desktop.yaml;
    wireguard.sopsFile = ./restic-desktop.yaml;
  };

  powerManagement.cpuFreqGovernor = "performance";

  # hardware.enableRedistributableFirmware = true;
  networking.hostName = "desktop"; # Define your hostname.
  networking.networkmanager.enable = false;
  programs.thefuck.enable = true;
  users.users.bbigras.packages = [ pkgs.retroarchBare ];

  services.flatpak.enable = true;
  services.smartd.enable = true;

  systemd.network = {
    enable = true;
    networks = {
      "10-lan" = {
        DHCP = "yes";
        matchConfig.Name = "enp3s0";
        # domains = [ "~." ];
        dhcpV4Config = { UseDNS = false; };
        dns = [
          # https://developers.cloudflare.com/1.1.1.1/dns-over-tls
          "1.1.1.1#cloudflare-dns.com"
          "1.0.0.1#cloudflare-dns.com"
        ];
        networkConfig = {
          # DNSSEC = true;
          DNSOverTLS = true;
        };
      };
    };
  };

  services.xserver = {
    autorun = true;
    displayManager.hiddenUsers = [ "builder" ];
    enable = true;
  };

  boot.supportedFilesystems = [ "ntfs" ];
  fileSystems."/media/gamedisk" = {
    device = "/dev/disk/by-uuid/A238EB6A38EB3BC3";
    fsType = "ntfs-3g";
    options = [ "rw" "uid=1001" ];
  };

  # networking.firewall.enable = false;
  networking.firewall.allowedTCPPorts = [
    9977
    9988
  ];
  #   22000
  #   6680
  #   51413 # transmission
  #   19515 # qbittorrent
  # ];
  # networking.firewall.allowedTCPPortRanges = [
  #   { from = 6881; to = 6999; } # aria2c
  # ];
  # networking.firewall.allowedUDPPorts = [
  #   21027
  # ];
  # networking.firewall.allowedUDPPortRanges = [
  #   { from = 6881; to = 6999; } # aria2c
  # ];

  users.users.builder = {
    createHome = true;
    isNormalUser = true;
  };

  # services.resilio = {
  #   enable = true;
  #   # openFirewall = true;
  #   enableWebUI = true;
  # };

  environment.systemPackages = with pkgs; [
    # boot.kernelPackages.bcc
  ];
  virtualisation.docker.enable = true;

  environment.persistence."/persist" = {
    hideMounts = true;
    directories = [
      "/var/lib/tailscale"
      "/var/lib/flatpak"
      "/var/lib/docker"
      "/var/lib/libvirt"
      "/root/.cache/restic"
      # "/var/cache/libvirt"

      #     # "/var/log"
      #     "/var/lib/bluetooth"
      #     "/var/lib/systemd/coredump"
      #     "/etc/NetworkManager/system-connections"
    ];
    files = [
      # "/etc/machine-id"
      # "/etc/nix/id_rsa"
    ];
  };

  # Note `lib.mkBefore` is used instead of `lib.mkAfter` here.
  boot.initrd.postDeviceCommands = pkgs.lib.mkBefore ''
    mkdir -p /mnt

    # We first mount the btrfs root to /mnt
    # so we can manipulate btrfs subvolumes.
    mount -o subvol=/ /dev/disk/by-uuid/407550f2-645a-41e6-9e35-ee3394f41e60 /mnt

    # While we're tempted to just delete /root and create
    # a new snapshot from /root-blank, /root is already
    # populated at this point with a number of subvolumes,
    # which makes `btrfs subvolume delete` fail.
    # So, we remove them first.
    #
    # /root contains subvolumes:
    # - /root/var/lib/portables
    # - /root/var/lib/machines
    #
    # I suspect these are related to systemd-nspawn, but
    # since I don't use it I'm not 100% sure.
    # Anyhow, deleting these subvolumes hasn't resulted
    # in any issues so far, except for fairly
    # benign-looking errors from systemd-tmpfiles.
    btrfs subvolume list -o /mnt/root |
    cut -f9 -d' ' |
    while read subvolume; do
      echo "deleting /$subvolume subvolume..."
      btrfs subvolume delete "/mnt/$subvolume"
    done &&
    echo "deleting /root subvolume..." &&
    btrfs subvolume delete /mnt/root

    echo "restoring blank /root subvolume..."
    btrfs subvolume snapshot /mnt/root-blank /mnt/root

    # Once we're done rolling back to a blank snapshot,
    # we can unmount /mnt and continue on the boot process.
    umount /mnt
  '';

  services.earlyoom.enable = true;

  services.btrbk = {
    instances = {
      home = {
        onCalendar = "hourly";
        settings = {
          timestamp_format = "long";
          snapshot_preserve = "48h";
          snapshot_preserve_min = "18h";
          volume = {
            "/mnt/btr_pool" = {
              subvolume = {
                home = {
                  snapshot_create = "always";
                };
              };
            };
          };
        };
      };
    };
  };

}
