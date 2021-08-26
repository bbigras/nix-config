{ pkgs, ... }:

{
  networking.firewall.allowedTCPPorts = [
    5977 # matrix-pinecone
  ];
  networking.firewall.allowedUDPPorts = [
    5977 # matrix-pinecone
    60606 # matrix-pinecone ipv6 multicast
  ];

  systemd.services."dendrite-demo-pinecone" = {
    enable = true;
    script = "${pkgs.dendrite-demo-pinecone}/bin/dendrite-demo-pinecone -peer ws://100.118.252.12:63998/ws -listen :5977";

    serviceConfig = {
      DynamicUser = true;
      StateDirectory = "dendrite-demo-pinecone";
      WorkingDirectory = "%S/dendrite-demo-pinecone";
    };

    wantedBy = [ "multi-user.target" ];
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
  };
}
