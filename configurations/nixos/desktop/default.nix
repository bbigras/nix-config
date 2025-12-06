# NixOS configuration for desktop
{
  flake,
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (flake) self;
in
{
  imports = [
    # Internal modules via flake outputs
    self.nixosModules.default
    self.nixosModules.users-bbigras
    self.nixosModules.graphical
    self.nixosModules.graphical-trusted
    self.nixosModules.pam-limits
    # self.nixosModules.hardware-secureboot
    self.nixosModules.hardware-nonsecureboot
    # self.nixosModules.hardware-thinkpad-z13
    # self.nixosModules.hardware-yubikey
    self.nixosModules.services-podman
    # self.nixosModules.services-virt-manager

    # Host-specific files
    # ./state.nix
    # ./cosmic.nix
  ];

  # Host-specific home-manager user config
  home-manager.users.bbigras.imports = [
    self.homeModules.trusted
    self.homeModules.trusted-graphical
    self.homeModules.asciinema
    self.homeModules.jjui
    self.homeModules.jujutsu
    self.homeModules.syncthing
    self.homeModules.graphical-firefox
    self.homeModules.graphical-keepassxc
    self.homeModules.graphical-lutris
    self.homeModules.graphical-mpv
    self.homeModules.graphical-vicinae
    self.homeModules.graphical-zed-editor
    self.homeModules.tkey-ssh-agent
    self.homeModules.tmux
  ];

  # SSH target for remote activation

  # Platform
  nixpkgs.hostPlatform = "x86_64-linux";

  # Host-specific configuration
  boot = {
    # extraModprobeConfig = ''
    #   options cfg80211 ieee80211_regdom="US"
    # '';
    initrd = {
      # kernelModules = [ "dm-snapshot" ];
      # luks.devices.cryptroot = {
      #   allowDiscards = true;
      #   bypassWorkqueues = true;
      #   device = "/dev/disk/by-uuid/75fa9c3c-3b95-479b-ad90-32d83528524d";
      # };
      systemd.enable = true;
    };
    plymouth.enable = true;
    # resumeDevice = "/dev/cryptroot/swap";
  };

  environment.systemPackages = with pkgs; [
    cntr
    wireguard-tools
  ];

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/e58653d8-7f76-402d-998d-400fe04f7520";
      fsType = "ext4";
    };
    "/boot" = {
      device = "/dev/disk/by-uuid/DA5A-BC65";
      fsType = "vfat";
      options = [
        "fmask=0022"
        "dmask=0022"
      ];
    };
    "/media/gamedisk" = {
      device = "/dev/disk/by-id/wwn-0x5000c5006527e1b4-part2";
      fsType = "ext4";
    };
  };

  # agenix-rekey host pubkey
  age.rekey.hostPubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINfn1kTx3Z2380QFj5IN/eWDe+/dt4CIzTaImlKbD+09";

  hardware = {
    # graphics.extraPackages = with pkgs; [
    #   libva-vdpau-driver
    #   libvdpau-va-gl
    # ];
  };

  networking = {
    # hostId = "a8766d75";
    hostName = "desktop";
    wireguard.enable = true;
  };

  nix.settings.max-substitution-jobs = 32;

  security.sudo.wheelNeedsPassword = true;

  services = {
    # btrfs.autoScrub = {
    #   enable = true;
    #   fileSystems = [ "/nix/state" ];
    #   interval = "weekly";
    # };
    # fstrim.enable = lib.mkForce false;
    udev.packages = with pkgs; [ logitech-udev-rules ];
    udisks2.enable = true;
    usbmuxd.enable = true;
  };

  systemd.network.networks = {
    lan = {
      DHCP = "yes";
      matchConfig.Name = "enp*";
      dhcpV4Config.RouteMetric = 20;
      dhcpV6Config.RouteMetric = 20;
      linkConfig = {
        Multicast = true;
        RequiredForOnline = "routable";
      };
      networkConfig = {
        MulticastDNS = true;
        LLMNR = true;
      };
    };
  };
}
