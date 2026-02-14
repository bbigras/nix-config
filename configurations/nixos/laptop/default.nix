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
    self.nixosModules.graphical-trusted
    self.nixosModules.pam-limits
    self.nixosModules.hardware-nonsecureboot
    self.nixosModules.services-podman
    self.nixosModules.services-wivrn
    self.nixosModules.services-virt-manager

    { config.facter.reportPath = ./facter.json; }
    inputs.nixos-hardware.nixosModules.framework-12-13th-gen-intel

    # Host-specific files
    # ./state.nix
    ./disko.nix
  ];

  # Host-specific home-manager user config
  home-manager.users.bbigras.imports = [
    self.homeModules.trusted
    self.homeModules.emacs
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

  # agenix-rekey host pubkey
  age.rekey.hostPubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGbvuozkVenMJeMFkpnnXA6qWTFfDhkjRq26VSgDSHme";

  networking = {
    # hostId = "";
    hostName = "laptop";
    wireguard.enable = true;
    wireless.enable = false;
  };

  nix.settings.max-substitution-jobs = 32;

  security.sudo.wheelNeedsPassword = true;

  services = {
    flatpak.enable = true;
    udisks2.enable = true;
  };

  networking.networkmanager = {
    enable = true;
    dns = "systemd-resolved";
    wifi = {
      backend = "iwd";
      macAddress = "stable-ssid";
      # powersave = false;
    };
    ensureProfiles = {
      environmentFiles = [
        # config.sops.secrets.networkmanager.path
      ];
      profiles = {
        home = {
          connection = {
            id = "home";
            type = "wifi";
            # uuid = "1a2538e1-1a72-4162-bc85-58682ecd91eb";
            autoconnect = true;
          };
          ipv4 = {
            method = "auto";
          };
          ipv6 = {
            addr-gen-mode = "default";
            method = "auto";
          };
          # proxy = { };
          wifi = {
            ssid = "$HOME_SSID";
          };
          wifi-security = {
            key-mgmt = "wpa-psk";
            psk = "$HOME_PASSPHRASE";
          };
        };
        phone = {
          connection = {
            autoconnect-priority = "-1";
            id = "phone";
            metered = "1";
            # timestamp = "1752765639";
            type = "wifi";
            # uuid = "d3c51cff-ccaa-481e-8d93-a02c5ac3bdda";
          };
          ipv4 = {
            method = "auto";
          };
          ipv6 = {
            addr-gen-mode = "stable-privacy";
            method = "auto";
          };
          # proxy = { };
          wifi = {
            # cloned-mac-address = "stable-ssid";
            mode = "infrastructure";
            ssid = "$PHONE_PASSPHRASE";
          };
          wifi-security = {
            key-mgmt = "wpa-psk";
            psk = "$PHONE_PSK";
          };
        };
      };
    };
  };

  # These options are unnecessary when managing DNS ourselves
  networking.useDHCP = false;
  networking.dhcpcd.enable = false;
  facter.detected.dhcp.enable = false;

  services = {
    fwupd = {
      enable = true;
      extraRemotes = [ "lvfs-testing" ];
      # uefiCapsuleSettings.DisableCapsuleUpdateOnDisk = true;
    };
    thermald.enable = true;
  };
}
