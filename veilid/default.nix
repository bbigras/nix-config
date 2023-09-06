{ pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [ veilid ];
  };

  networking = {
    firewall = {
      allowedTCPPorts = [
        5150
      ];
      allowedUDPPorts = [
        5150
      ];
    };
  };

  systemd.services.veilid = {
    description = "Veilid Network Service";
    after = [ "network-pre.target" ];
    wants = [ "network.target" ];
    before = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];

    script = "${pkgs.veilid}/bin/veilid-server";

    environment = {
      HOME = "/var/lib/veilid";
    };

    serviceConfig = {
      User = "veilid";
      Restart = "always";
      StateDirectory = "veilid";
      RuntimeDirectory = "veilid";
      # RuntimeDirectoryMode = "0750";
    };
  };

  users.users.veilid = {
    isSystemUser = true;
  };

  users.users.veilid.group = "veilid";
  users.groups.veilid = { };
}
