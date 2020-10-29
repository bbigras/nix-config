# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ pkgs, ... }:
let
  zen = (import (import ../nix).zen { });
in
{
  imports =
    [
      ../core
      # ../dev

      (import ../nix).cpu_intel
      (import ../nix).ssd
      (import ((import ../nix).impermanence + "/nixos.nix"))

      # Include the results of the hardware scan.
      ../hardware/hardware-configuration-desktop.nix
      ../hardware/efi.nix

      ../dev/qemu.nix
      ../dev/virt-manager.nix

      # ../configuration_common.nix
      # ./aarch64.nix

      ../gnome.nix

      ../users/bbigras
    ] ++ (if builtins.pathExists ../secrets/at_home.nix then [ ../secrets/at_home.nix ] else [ ]);

  boot.kernelPackages = zen.linuxPackages_zen;
  boot.loader.grub.useOSProber = true;
  # hardware.enableRedistributableFirmware = true;
  networking.hostName = "desktop"; # Define your hostname.
  programs.thefuck.enable = true;
  time.hardwareClockInLocalTime = true;
  users.users.bbigras.packages = [ pkgs.retroarchBare ];

  networking.networkmanager.enable = false;

  systemd.network = {
    enable = true;
    networks = {
      lan = {
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
    videoDrivers = [ "nvidia" ];
  };

  boot.kernel.sysctl = {
    "kernel.sysrq" = 1;
    # "fs.inotify.max_user_watches" = 524288;
    # "vm.swappiness" = 1;
  };

  fileSystems."/media/gamedisk" =
    {
      device = "/dev/disk/by-uuid/A238EB6A38EB3BC3";
      fsType = "ntfs";
      options = [ "uid=1000,gid=100,rw,user,exec,umask=000" ];
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

  environment.persistence."/persist" = {
    directories = [
      "/var/lib/zerotier-one"
      "/var/lib/tailscale"
      "/var/lib/jellyfin"
      "/var/lib/docker"
      "/var/lib/libvirt"
      # "/var/cache/jellyfin"
      # "/var/cache/libvirt"

      #     # "/var/log"
      #     "/var/lib/bluetooth"
      #     "/var/lib/systemd/coredump"
      #     "/etc/NetworkManager/system-connections"
    ];
    files = [
      "/etc/machine-id"
      # "/etc/nix/id_rsa"

      "/etc/ssh/ssh_host_ed25519_key"
      "/etc/ssh/ssh_host_ed25519_key.pub"
      "/etc/ssh/ssh_host_rsa_key"
      "/etc/ssh/ssh_host_rsa_key.pub"
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

  environment.etc."restic-pw-id".text = ''
    BW_ID=ca6ebfdf-1d09-4631-9531-ab3d004496c8
  '';

  services.earlyoom.enable = true;
  services.jellyfin.enable = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.09"; # Did you read the comment?

}
