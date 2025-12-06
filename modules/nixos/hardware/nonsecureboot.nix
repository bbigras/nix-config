{
  config,
  lib,
  pkgs,
  ...
}:
{
  boot = {
    loader.systemd-boot.enable = true;
    loader.grub.enable = false;
  };
}
