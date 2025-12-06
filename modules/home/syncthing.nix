{
  # networking = {
  #   firewall = {
  #     allowedUDPPorts = [
  #       22000 # syncthing
  #       21027 # syncthing discovery
  #     ];
  #     allowedTCPPorts = [
  #       22000 # syncthing
  #     ];
  #   };
  # };

  services.syncthing.enable = true;
  systemd.user.services = {
    syncthing = {
      Service = {
        StandardOutput = "null";
      };
    };
  };
}
