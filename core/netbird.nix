{
  services.netbird.enable = true;

  networking = {
    firewall = {
      allowedUDPPorts = [ 51820 ];
      checkReversePath = "loose";
      trustedInterfaces = [ "wt0" ];
    };
  };
}
