{ lib, ... }:
{
  boot.loader = {
    efi.canTouchEfiVariables = true;
    systemd-boot = {
      enable = true;
      configurationLimit = 4;
    };
    timeout = lib.mkDefault 0;
  };
  console.earlySetup = true;
}
