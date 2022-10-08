{ lib, ... }: {
  networking = {
    firewall = {
      allowedTCPPorts = [ 5355 ];
      allowedUDPPorts = [ 5353 5355 ];
    };
    networkmanager.dns = "systemd-resolved";
  };

  services.resolved = {
    enable = true;
    dnssec = "allow-downgrade";
    llmnr = "true";
    extraConfig = ''
      DNS=1.1.1.1 1.0.0.1
      Domains=~.
      MulticastDNS=true
    '';
  };

  system.nssDatabases.hosts = lib.mkMerge [
    (lib.mkBefore [ "mdns_minimal [NOTFOUND=return]" ])
    (lib.mkAfter [ "mdns" ])
  ];
}
