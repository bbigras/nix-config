{ pkgs, ... }:

{
  networking.firewall.allowedTCPPorts = [
    5977 # matrix-pinecone
  ];
  networking.firewall.allowedUDPPorts = [
    5977 # matrix-pinecone
    60606 # matrix-pinecone ipv6 multicast
  ];

  systemd.services."nix-matrix-pinecone" = {
    enable = true;
    script = "${pkgs.nix-matrix-pinecone}/bin/dendrite-demo-pinecone -peer wss://pinecone.matrix.org/public -listen :5977";

    serviceConfig = {
      DynamicUser = true;
      StateDirectory = "nix-matrix-pinecone";
      WorkingDirectory = "%S/nix-matrix-pinecone";
    };

    wantedBy = [ "multi-user.target" ];
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
  };
}
