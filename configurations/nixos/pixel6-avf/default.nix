# NixOS configuration for pixel6-avf
{
  flake,
  lib,
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
    self.nixosModules.graphical-fonts

    inputs.nixos-avf.nixosModules.avf
  ];

  # Host-specific home-manager user config
  home-manager.users.bbigras.imports = [
    self.homeModules.tkey-ssh-agent
  ];

  home-manager.users.bbigras = {
    services = {
      gpg-agent.enable = true;
      gpg-agent.pinentry = {
        package = pkgs.pinentry-tty;
        program = "pinentry-tty";
      };
    };
  };

  # for avf
  avf.defaultUser = "bbigras";
  networking.useDHCP = lib.mkForce true;
  services.resolved.settings.Resolve.DNSSEC = lib.mkForce "false";

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
