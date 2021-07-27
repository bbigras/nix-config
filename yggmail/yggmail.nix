{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.services.yggmail;
in
{
  options.services.yggmail = {
    enable = mkEnableOption "yggmail mailserver";

    peer = mkOption {
      #default = "";
      type = types.str;
      default = "tcp://127.0.0.1:6893";
      example = "tcp://127.0.0.1:6893";
      description = ''
        Connect to a specific Yggdrasil static peer
      '';
    };

    imapPort = mkOption {
      default = "localhost:1143";
      example = "localhost:1143";
      type = types.str;
      description = ''
        Listen port for IMAP
      '';
    };

    smtpPort = mkOption {
      default = "localhost:1025";
      example = "localhost:1025";
      type = types.str;
      description = ''
        Listen port for SMTP
      '';
    };
  };
  config = mkIf cfg.enable {
    systemd.services.yggmail = {
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      description = "Start the yggmail daemon.";
      serviceConfig = {
        DynamicUser = true;
        #AmbientCapabilities = [ "CAP_NET_BIND_SERVICE" ];
        #CapabilityBoundingSet = [ "CAP_NET_BIND_SERVICE" ];
        ExecStart = ''${pkgs.yggmail}/bin/yggmail -imap=${cfg.imapPort} -smtp=${cfg.smtpPort} -peer=${cfg.peer}'';
        Restart = "always";
        StateDirectory = "yggmail";
        WorkingDirectory = "/var/lib/yggmail";
      };
    };
  };
}

