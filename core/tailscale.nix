{ config, ... }:

{
  services.tailscale.enable = true;
  services.tailscale.extraDaemonFlags = [
    "--no-logs-no-support"
  ];
  services.tailscale.extraSetFlags = [
    "--netfilter-mode=on"
  ];

  systemd = {
    services.tailscaled = {
      after = [
        "network-online.target"
        "systemd-resolved.service"
      ];
      wants = [
        "network-online.target"
        "systemd-resolved.service"
      ];
    };
  };

  networking = {
    firewall = {
      allowedUDPPorts = [ config.services.tailscale.port ];
      checkReversePath = "loose";
      trustedInterfaces = [ "tailscale0" ];
    };
  };
}
