{ lib, ... }: {
  services.tailscale.enable = true;
  networking.firewall.interfaces."tailscale0".allowedTCPPorts = [ 22 ];
}
