{
  lib,
  pkgs,
  ...
}:
{
  boot = {
    bootspec.enable = true;
    lanzaboote = {
      enable = true;
      autoGenerateKeys.enable = true;
      autoEnrollKeys = {
        enable = true;
        # Automatically reboot to enroll the keys in the firmware
        autoReboot = true;
      };
      pkiBundle = "/etc/secureboot";
    };
    loader.systemd-boot.enable = lib.mkForce false;
    loader.grub.enable = lib.mkForce false;
  };

  environment.systemPackages = with pkgs; [ sbctl ];

  # environment.persistence."/nix/state".directories = [ config.boot.lanzaboote.pkiBundle ];
}
