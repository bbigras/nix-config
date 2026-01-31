# NixOS configuration for pixel6-avf
{
  flake,
  lib,
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

    inputs.nixos-avf.nixosModules.avf
  ];

  # Host-specific home-manager user config
  home-manager.users.bbigras.imports = [
    self.homeModules.tkey-ssh-agent
  ];

  # for avf
  avf.defaultUser = "bbigras";
  networking.useDHCP = lib.mkForce true;
  services.resolved.dnssec = lib.mkForce "false";

  # SSH target for remote activation

  # Platform
  nixpkgs.hostPlatform = "aarch64-linux";

  # agenix-rekey host pubkey
  age.rekey.hostPubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIOVUxOP3wQ8g+Ml+B5MWj+m11DoqMT1bDSv6abOtK1M";

  networking = {
    # hostId = "";
    hostName = "pixel6-avf";
  };
}
