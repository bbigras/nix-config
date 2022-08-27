{ pkgs, lib, config, ... }:

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

  systemd.services.yggdrasil = {
    serviceConfig.DynamicUser = lib.mkForce false;
    serviceConfig.SupplementaryGroups = [ config.users.groups.keys.name ];
  };

  systemd.services.yggdrasil.serviceConfig.User = config.users.users.yggdrasil.name;
  systemd.services.yggdrasil.serviceConfig.Group = "yggdrasil";

  users.users.yggdrasil = {
    isSystemUser = true;
  };

  users.users.yggdrasil.group = "yggdrasil";
  users.groups.yggdrasil = { };
}
