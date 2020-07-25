# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
  imports =
    [
      ../core

      # Include the results of the hardware scan.
      ../hardware/hardware-configuration-laptop.nix
      ../hardware/efi.nix
      ../hardware/bluetooth.nix

      (import ../nix).xps-13-9343

      ../sway
      ../sway/trusted.nix

      ../users/bbigras
    ];

  hardware.brillo.enable = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;
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
  services.snapper = {
    filters = "/nix";
    configs.home = {
      subvolume = "/home";
      extraConfig = ''
        TIMELINE_CREATE="yes"
        ALLOW_USERS="bbigras"
      '';
    };
  };

  fileSystems."/" =
    {
      device = "/dev/disk/by-uuid/779232c6-776b-4d0a-b52a-f6d26a40508e";
      fsType = "btrfs";
      options = [ "subvol=@,compress=zstd,noatime" ];
    };

  fileSystems."/home" =
    {
      device = "/dev/disk/by-uuid/779232c6-776b-4d0a-b52a-f6d26a40508e";
      fsType = "btrfs";
      options = [ "subvol=@home,compress=zstd,noatime" ];
    };

  fileSystems."/nix" =
    {
      device = "/dev/disk/by-uuid/779232c6-776b-4d0a-b52a-f6d26a40508e";
      fsType = "btrfs";
      options = [ "subvol=@nix,compress=zstd,noatime" ];
    };

  fileSystems."/persist" =
    {
      device = "/dev/disk/by-uuid/779232c6-776b-4d0a-b52a-f6d26a40508e";
      fsType = "btrfs";
      options = [ "subvol=@persist,compress=zstd,noatime" ];
    };

  fileSystems."/snapshot" =
    {
      device = "/dev/disk/by-uuid/779232c6-776b-4d0a-b52a-f6d26a40508e";
      fsType = "btrfs";
      options = [ "subvol=@snapshot,compress=zstd,noatime" ];
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
