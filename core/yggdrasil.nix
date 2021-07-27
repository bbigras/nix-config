{ pkgs, config, ... }:

{
  networking.firewall.allowedTCPPorts = [ 9002 ];
  networking.firewall.allowedUDPPorts = [ 9001 ];

  environment = {
    systemPackages = with pkgs; [ yggdrasil ];
  };

  services.yggdrasil = {
    enable = true;
    configFile = config.sops.secrets.yggdrasil-conf.path;
  };
}
