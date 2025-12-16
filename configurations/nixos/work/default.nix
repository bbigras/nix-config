# NixOS configuration for work
{
  flake,
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
    self.nixosModules.pam-limits
    self.nixosModules.hardware-nonsecureboot
    self.nixosModules.services-podman
    self.nixosModules.services-virt-manager

    # Host-specific files
    # ./state.nix
    { config.facter.reportPath = ./facter.json; }
  ];

  # Host-specific home-manager user config
  home-manager.users.bbigras.imports = [
    self.homeModules.trusted
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

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/ccb4db0b-1cb8-4d5f-be9a-8b20a5c63982";
    fsType = "btrfs";
    options = [
      "subvol=nixos"
      "compress=zstd"
    ];
  };

  # agenix-rekey host pubkey
  age.rekey.hostPubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKEfhY66gBDU0xgjaQgm9V991wuxI/R3bm3Yt6Kdv9Au";

  networking = {
    # hostId = "";
    hostName = "bbigras-work";
    wireguard.enable = true;
  };

  # nix.settings.max-substitution-jobs = 32;

  security.sudo.wheelNeedsPassword = true;

  services = {
    fwupd.enable = true;
    udisks2.enable = true;
  };

}
