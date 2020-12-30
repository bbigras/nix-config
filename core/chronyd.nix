{ pkgs, ... }:
let
  configFile = pkgs.writeText "chrony.conf" ''
    server time.cloudflare.com iburst nts

    initstepslew 1000 time.cloudflare.com

    driftfile /var/lib/chrony/chrony.drift
    keyfile /var/lib/chrony/chrony.keys

    ntsdumpdir /var/lib/chrony/nts
  '';
in
{
  services.timesyncd.enable = false;

  users.groups.chrony.gid = 61;

  users.users.chrony =
    {
      uid = 61;
      group = "chrony";
      description = "chrony daemon user";
      home = "/var/lib/chrony";
    };

  systemd = {
    services = {
      systemd-timedated.environment = { SYSTEMD_TIMEDATED_NTP_SERVICES = "chronyd.service"; };
      chronyd =
        {
          description = "chrony NTP daemon";

          wantedBy = [ "multi-user.target" ];
          wants = [ "time-sync.target" ];
          before = [ "time-sync.target" ];
          after = [ "network.target" ];
          conflicts = [ "ntpd.service" "systemd-timesyncd.service" ];

          path = [ pkgs.chrony ];

          unitConfig.ConditionCapability = "CAP_SYS_TIME";
          serviceConfig =
            {
              Type = "simple";
              ExecStart = "${pkgs.chrony}/bin/chronyd -n -m -u chrony -f ${configFile}";

              ProtectHome = "yes";
              ProtectSystem = "full";
              PrivateTmp = "yes";
            };
        };
    };
    tmpfiles.rules = [
      "d /var/lib/chrony 0755 chrony chrony - -"
      "f /var/lib/chrony/chrony.keys 0640 chrony chrony -"
    ];
  };

  environment.systemPackages = with pkgs; [ chrony ];
}
