{ lib, ... }:
{
  programs.mosh.enable = true;

  services.openssh = {
    enable = true;
    settings = {
      KbdInteractiveAuthentication = false;
      PasswordAuthentication = false;
      PermitRootLogin = lib.mkDefault "no";
    };
    startWhenNeeded = true;
    openFirewall = true;
  };
}
