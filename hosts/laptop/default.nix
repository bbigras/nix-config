# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ pkgs, inputs, ... }:

{
  imports =
    [
      ../../core

      # Include the results of the hardware scan.
      ../../hardware/hardware-configuration-laptop.nix
      ../../hardware/efi.nix
      ../../hardware/bluetooth.nix
      inputs.nixos-hardware.nixosModules.dell-xps-13-9343

      ../../sway
      ../../sway/trusted.nix

      ../../users/bbigras
    ] ++ (if builtins.pathExists (builtins.getEnv "PWD" + "/secrets/at_home.nix") then [ (builtins.getEnv "PWD" + "/secrets/at_home.nix") ] else [ ])
    ++ (if builtins.pathExists (builtins.getEnv "PWD" + "/secrets/laptop.nix") then [ (builtins.getEnv "PWD" + "/secrets/laptop.nix") ] else [ ]);

  environment.systemPackages = with pkgs; [
    iwd
  ];

  sops.secrets.restic-laptop-password.sopsFile = ./restic-laptop.yaml;
  sops.secrets.restic-laptop-creds.sopsFile = ./restic-laptop.yaml;

  services.auto-cpufreq.enable = true;

  hardware.brillo.enable = true;
  # boot.kernelPackages = pkgs.linuxPackages_zen;
  boot.kernel.sysctl = {
    "kernel.sysrq" = 1;
    # "fs.inotify.max_user_watches" = 524288;
    # "vm.swappiness" = 1;
  };

  networking = {
    useNetworkd = true;
    dhcpcd.enable = false;
    hostName = "laptop"; # Define your hostname.
    networkmanager.enable = false;
    wireless.iwd.enable = true;
    useDHCP = false;
    interfaces.eth0.useDHCP = true;
  };

  xdg.portal = {
    enable = true;
    gtkUsePortal = true;
    extraPortals = with pkgs;
      [ xdg-desktop-portal-wlr xdg-desktop-portal-gtk ];
  };

  services.acpid = {
    enable = true;
    handlers = {
      ac-power = {
        action = ''
          vals=($1)  # space separated string to array of multiple values
          case ''${vals[3]} in
              00000000)
                  echo unplugged >> /tmp/acpi.log
                  ${pkgs.brillo}/bin/brillo -e -S 50
                  ;;
              00000001)
                  echo plugged in >> /tmp/acpi.log
                  ${pkgs.brillo}/bin/brillo -e -S 100
                  ;;
              *)
                  echo unknown >> /tmp/acpi.log
                  ;;
          esac
        '';
        event = "ac_adapter/*";
      };
    };
  };

  fileSystems."/" =
    {
      device = "/dev/disk/by-uuid/8e2cf716-7b2f-4c87-a895-1ea6d84d5f65";
      fsType = "btrfs";
      options = [ "subvol=root,compress=zstd,noatime" ];
    };

  fileSystems."/nix" =
    {
      device = "/dev/disk/by-uuid/8e2cf716-7b2f-4c87-a895-1ea6d84d5f65";
      fsType = "btrfs";
      options = [ "subvol=nix,compress=zstd,noatime" ];
    };

  fileSystems."/persist" =
    {
      device = "/dev/disk/by-uuid/8e2cf716-7b2f-4c87-a895-1ea6d84d5f65";
      fsType = "btrfs";
      options = [ "subvol=persist,compress=zstd,noatime" ];
      neededForBoot = true;
    };

  # systemd.user.services.mpris-proxy = {
  #   description = "Mpris proxy";
  #   after = [ "network.target" "sound.target" ];
  #   script = "${pkgs.bluez}/bin/mpris-proxy";
  #   wantedBy = [ "default.target" ];
  # };

  services = {
    # fstrim.enable = true;
    # fwupd.enable = true;
    tlp = {
      enable = true;
      extraConfig = ''
        CPU_BOOST_ON_AC=0
        CPU_BOOST_ON_BAT=0
      '';
    };
  };

  environment.persistence."/persist" = {
    directories = [
      "/var/lib/iwd"
      "/var/lib/tailscale"
      "/var/lib/zerotier-one"
      "/var/log"
      "/var/lib/flatpak"
      "/var/lib/docker"
      "/var/lib/libvirt"
      "/home/bbigras/.dropbox-dist"
      "/home/bbigras/.dropbox-hm"
      "/root/.cache/restic"
      # "/var/cache/libvirt"
      #     "/var/lib/bluetooth"
      #     "/var/lib/systemd/coredump"
    ];
    files = [
      "/etc/machine-id"
      "/etc/ssh/ssh_host_ed25519_key"
      "/etc/ssh/ssh_host_ed25519_key.pub"
      "/etc/ssh/ssh_host_rsa_key"
      "/etc/ssh/ssh_host_rsa_key.pub"
      # "/etc/nix/id_rsa"
    ];
  };

  home-manager.users.bbigras = { ... }: {
    imports = [
      "${inputs.impermanence}/home-manager.nix"
    ];

    wayland.windowManager.sway = {
      config = {
        input = {
          "1:1:AT_Translated_Set_2_keyboard" = {
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
        };

        output = {
          "*" = {
            scale = "2";
            # bg = "~/Downloads/molly.png fit";
          };
        };
      };
    };

    home.persistence."/persist/home/bbigras" = {
      directories = [
        ".cache/mozilla"
        ".cache/restic"
        ".cache/tealdeer"
        ".cargo"
        ".config/Bitwarden CLI"
        ".config/discord"
        ".config/syncthing"
        ".wrangler/config"
        ".cache/.wrangler"
        ".cache/wine"
        ".cache/winetricks"
        ".npm/_cacache/index-v5"
        ".npm/_cacache/content-v2"
        ".gnupg/private-keys-v1.d"
        ".local/share/direnv"
        ".local/share/keyrings"
        ".local/share/remmina"
        ".local/share/zoxide"
        ".local/share/Steam"
        ".mozilla"
        ".steam"
        "Documents"
        "Downloads"
        "Maildir"
        "Music"
        "Pictures"
        "Videos"
        "dev"
        "matrix-p2p"
        "tmp"
        "src"
        # ".nixops"
        # ".ssh"
        # "VirtualBox VMs"
      ];
      files = [
        ".authinfo.gpg"
        ".cache/swaymenu-history.txt"
        ".config/cachix/cachix.dhall"
        ".config/remmina/remmina.pref"
        ".gnupg/pubring.kbx"
        ".gnupg/random_seed"
        ".gnupg/sshcontrol"
        ".gnupg/trustdb.gpg"
        # ".local/share/fish/fish_history"
        ".mcfly/history.db"
        ".notmuch-config"
        ".ssh/id_ed25519"
        ".ssh/id_ed25519.pub"
        ".ssh/known_hosts"
        # ".cache/cargo/credentials"
        # ".config/dconf"
        # ".local/share/keyrings"
      ];
    };
  };

  # Note `lib.mkBefore` is used instead of `lib.mkAfter` here.
  boot.initrd.postDeviceCommands = pkgs.lib.mkBefore ''
    mkdir -p /mnt

    # We first mount the btrfs root to /mnt
    # so we can manipulate btrfs subvolumes.
    mount -o subvol=/ /dev/mapper/enc /mnt

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
}
