# NixOS configuration for desktop
{
  flake,
  pkgs,
  ...
}:
let
  inherit (flake) inputs self;
in
{
  imports = [
    # Internal modules via flake outputs
    self.nixosModules.default
    self.nixosModules.users-bbigras
    self.nixosModules.graphical
    self.nixosModules.graphical-cosmic
    self.nixosModules.graphical-steam
    self.nixosModules.graphical-fonts
    self.nixosModules.graphical-trusted
    self.nixosModules.pam-limits
    self.nixosModules.hardware-nonsecureboot
    self.nixosModules.services-podman
    self.nixosModules.services-wivrn
    self.nixosModules.services-virt-manager

    # Hardware modules from nixos-hardware
    inputs.nixos-hardware.nixosModules.common-cpu-intel
    inputs.nixos-hardware.nixosModules.common-gpu-amd
    inputs.nixos-hardware.nixosModules.common-pc-ssd

    # Host-specific files
    # ./state.nix
    { config.facter.reportPath = ./facter.json; }
  ];

  hardware.amdgpu.initrd.enable = true;

  # Host-specific home-manager user config
  home-manager.users.bbigras.imports = [
    self.homeModules.trusted
    self.homeModules.emacs
    self.homeModules.radicle
    self.homeModules.syncthing
    self.homeModules.graphical
    self.homeModules.graphical-heroic
    self.homeModules.graphical-lutris
    self.homeModules.tkey-ssh-agent
  ];

  # SSH target for remote activation

  # Platform
  nixpkgs.hostPlatform = "x86_64-linux";

  # Host-specific configuration
  boot = {
    initrd = {
      systemd.enable = true;
    };
    plymouth.enable = true;
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

  networking = {
    # hostId = "";
    hostName = "desktop";
    wireguard.enable = true;
  };

  nix.settings.max-substitution-jobs = 32;

  security.sudo.wheelNeedsPassword = true;

  services = {
    flatpak.enable = true;
    fwupd.enable = true;
    udisks2.enable = true;
    avahi.enable = pkgs.lib.mkForce false;
    earlyoom = {
      enable = true;
      enableNotifications = true;
    };
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
