# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:
let
  zen = (import (import ../nix).zen { });
in
{
  imports =
    [
      ../core

      (import ((import ../nix).impermanence + "/nixos.nix"))

      # Include the results of the hardware scan.
      ../hardware/hardware-configuration-laptop.nix
      ../hardware/efi.nix
      ../hardware/bluetooth.nix

      (import ../nix).xps-13-9343

      ../sway
      ../sway/trusted.nix

      ../users/bbigras
    ] ++ (if builtins.pathExists ../secrets/at_home.nix then [ ../secrets/at_home.nix ] else [ ]);

  hardware.brillo.enable = true;
  boot.kernelPackages = zen.linuxPackages_zen;
  boot.kernel.sysctl = {
    "kernel.sysrq" = 1;
    # "fs.inotify.max_user_watches" = 524288;
    # "vm.swappiness" = 1;
  };

  networking = {
    dhcpcd.enable = false;
    hostName = "laptop"; # Define your hostname.
    networkmanager.enable = true;
    useDHCP = false;
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
        CPU_ENERGY_PERF_POLICY_ON_AC=performance
        CPU_ENERGY_PERF_POLICY_ON_BAT=power
        CPU_MAX_PERF_ON_AC=100
        CPU_MAX_PERF_ON_BAT=50
        CPU_SCALING_GOVERNOR_ON_AC=performance
        CPU_SCALING_GOVERNOR_ON_BAT=powersave

        DEVICES_TO_DISABLE_ON_BAT_NOT_IN_USE="bluetooth wifi"
        DEVICES_TO_ENABLE_ON_AC="bluetooth wifi"

        DISK_DEVICES="sda"
        DISK_IOSCHED="mq-deadline bfq"

        MAX_LOST_WORK_SECS_ON_AC=15
        MAX_LOST_WORK_SECS_ON_BAT=15

        RUNTIME_PM_ON_AC=auto
        RUNTIME_PM_ON_BAT=auto

        SOUND_POWER_SAVE_ON_AC=1
        SOUND_POWER_SAVE_ON_BAT=1
        SOUND_POWER_SAVE_CONTROLLER=Y
      '';
    };
  };

  # DISK_APM_LEVEL_ON_AC="255 254"
  # DISK_APM_LEVEL_ON_BAT="128 127"

  environment.persistence."/persist" = {
    directories = [
      "/etc/NetworkManager/system-connections"
      "/var/lib/tailscale"
      "/var/lib/zerotier-one"
      "/var/log"
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
    imports = [ (import ((import ../nix).impermanence + "/home-manager.nix")) ];
    home.persistence."/persist/home/bbigras" = {
      directories = [
        ".cache/mozilla"
        ".cache/restic"
        ".cache/tealdeer"
        ".cargo"
        ".config/Bitwarden CLI"
        ".config/discord"
        ".dropbox-dist"
        ".dropbox-hm"
        ".gnupg/private-keys-v1.d"
        ".local/share/direnv"
        ".local/share/keyrings"
        ".local/share/remmina"
        ".local/share/zoxide"
        ".mozilla"
        "Documents"
        "Downloads"
        "Maildir"
        "Music"
        "Pictures"
        "Videos"
        "dev"
        "matrix-p2p"
        "tmp"
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
        ".local/share/fish/fish_history"
        ".notmuch-config"
        ".ssh/id_ed25519"
        ".ssh/id_ed25519.pub"
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

  home-manager.verbose = true;
  environment.etc."restic-pw-id".text = ''
    BW_ID=873078f3-3587-40ed-8ecc-aba30019a273
  '';

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?
}
