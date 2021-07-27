let
  imapPort = 1143;
  smtpPort = 1025;
in
{
  services.yggmail = {
    enable = true;
    peer = "tcp://127.0.0.1:9002";
    imapPort = "0.0.0.0:${toString imapPort}";
    smtpPort = "0.0.0.0:${toString smtpPort}";
  };

  networking.firewall.interfaces."tailscale0".allowedTCPPorts = [
    imapPort
    smtpPort
  ];
}
