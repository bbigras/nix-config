{ config, ... }:

{
  services.tailscale.enable = true;
  systemd.services.tailscaled.after = [ "network-online.target" "systemd-resolved.service" ];

  networking = {
    firewall = {
      allowedUDPPorts = [ config.services.tailscale.port ];
      checkReversePath = "loose";
      trustedInterfaces = [ "tailscale0" ];
    };
  };
}
