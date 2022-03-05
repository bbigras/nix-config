{ lib, ... }: {
  programs.mosh.enable = true;

  services.openssh = {
    enable = true;
    kbdInteractiveAuthentication = false;
    permitRootLogin = lib.mkDefault "no";
    startWhenNeeded = true;
    openFirewall = false;
  };
}
