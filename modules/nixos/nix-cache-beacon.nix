{
  services.nix-cache-beacon = {
    # Announce cache to the local network
    advert = {
      enable = true;
      port = 5000; # Harmonia port
    };

    # Enable local binary cache using discovered caches on the local network
    cache.enable = true;
  };

  # Make Nix aware of our local network cache
  nix.settings.substituters = [ "http://localhost:5028" ];

  # Local binary cache using Harmonia
  # nix-cache-beacon can be used with any cache implementation
  services.harmonia.cache.enable = true; # Serve up local Nix store
  networking.firewall.allowedTCPPorts = [ 5000 ]; # Open firewall port for Harmonia
}
