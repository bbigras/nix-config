{lib, ...}:

{
  services.ncro = {
    enable = true;
    settings = {
      # logging.level = "debug";
      discovery.enabled = true;
      upstreams = [
        {
          url = "https://cache.nixos.org";
          priority = 10;
          public_key = "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=";
        }

        {
          url = "https://nix-community.cachix.org";
          priority = 20;
          public_key = "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=";
        }

        {
          url = "https://bbigras-nix-config.cachix.org";
          priority = 30;
          public_key = "bbigras-nix-config.cachix.org-1:aXL6Q9Oi0jyF79YAKRu17iVNk9HY0p23OZX7FA8ulhU=";
        }

        {
          url = "http://10.10.0.51:8501";
          priority = 40;
          public_key = "10.10.0.51:zSAiwQJTX02yGP2NSof1Pin339R5YP+91Y5xdaqFsnU=";
        }
      ];
    };
  };

  # Point Nix at the proxy. By default the module appends every configured
  # upstream public_key to nix.settings.trusted-public-keys; set
  # services.ncro.addUpstreamPublicKeys = false to manage those keys yourself.
  # NOTE: ncro needs to be the *only* substituter if you wish to benefit
  # from its capabilities fully. If there are other substituters in your
  # list, or if you don't mkForce this option, ncro will perform less
  # efficiently.
  nix.settings.substituters = lib.mkForce [ "http://localhost:8080" ];
}
