{
  services.resolved = {
    enable = true;
    dnssec = "false";
    # dnssec = "allow-downgrade";
    # DNSOverTLS=yes
    llmnr = "false";

    extraConfig = ''
      DNS=1.1.1.1#cloudflare-dns.com 1.0.0.1#cloudflare-dns.com
      DNSOverTLS=true
    '';
  };
}
