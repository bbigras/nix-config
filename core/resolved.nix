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
    extraConfig = ''
      DNS=1.1.1.1 8.8.8.8
      Domains=~.
      LLMNR=true
      MulticastDNS=true
    '';
  };

  system.nssDatabases.hosts = lib.mkMerge [
    (lib.mkBefore [ "mdns_minimal [NOTFOUND=return]" ])
    (lib.mkAfter [ "mdns" ])
  ];
}
