{ lib, ... }:
{
  networking = {
    firewall = {
      allowedTCPPorts = [ 5355 ];
      allowedUDPPorts = [
        5353
        5355
      ];
    };
    networkmanager.dns = "systemd-resolved";
  };

  services.resolved = {
    enable = true;
    dnssec = "allow-downgrade";
    dnsovertls = "true";
    fallbackDns = [
      "1.1.1.1#cloudflare-dns.com"
      "1.0.0.1#cloudflare-dns.com"
    ];
    llmnr = "true";
    extraConfig = ''
      Domains=~.
      MulticastDNS=true
    '';
  };

  system.nssDatabases.hosts = lib.mkMerge [
    (lib.mkBefore [ "mdns_minimal [NOTFOUND=return]" ])
    (lib.mkAfter [ "mdns" ])
  ];
}
