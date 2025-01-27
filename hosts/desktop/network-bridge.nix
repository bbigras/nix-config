{ lib, ... }:
{
  systemd.network = {
    netdevs = {
      # Create the bridge interface
      "20-br0" = {
        netdevConfig = {
          Kind = "bridge";
          Name = "br0";
        };
      };
    };
    networks = {
      "10-lan" = {
        # matchConfig.Name = "enp6s0";
        matchConfig.Name = "enp*";
        networkConfig.Bridge = "br0";
        linkConfig.RequiredForOnline = lib.mkForce "enslaved";
      };
      # Configure the bridge for its desired function
      "40-br0" = {
        matchConfig.Name = "br0";
        bridgeConfig = { };
        gateway = [ "192.168.68.1" ];
        dns = [
          "1.1.1.1"
          "1.0.0.1"
        ];
        address = [ "192.168.68.68/24" ];
        # Disable address autoconfig when no IP configuration is required
        #networkConfig.LinkLocalAddressing = "no";
        linkConfig = {
          # or "routable" with IP addresses configured
          # RequiredForOnline = "carrier";
          RequiredForOnline = "routable";
        };
      };
    };
  };
}
