{ pkgs, ... }: {
  nix = {
    allowedUsers = [ "@wheel" ];
    daemonCPUSchedPolicy = "batch";
    daemonIOSchedPriority = 5;
    # nrBuildUsers = config.nix.maxJobs * 2;
    # optimise = {
    #   automatic = true;
    #   dates = [ "01:10" "12:10" ];
    # };
    trustedUsers = [ "root" "@wheel" ];

    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes ca-derivations ca-references
      keep-outputs = true
      keep-derivations = true
    '';

    binaryCaches = [
      "https://nix-community.cachix.org"
      "https://dendrite-demo-pinecone.cachix.org"
      "https://bbigras-nix-config.cachix.org"
      "https://mjlbach.cachix.org"
      "https://pre-commit-hooks.cachix.org"
      "https://bbigras-dev.cachix.org"
      "https://cache.ngi0.nixos.org"
      "https://nix-on-droid.cachix.org"
    ];
    binaryCachePublicKeys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "dendrite-demo-pinecone.cachix.org-1:qgybhOM1X0JikTrvpYo1HwtsXT2ee+6ajbmCjCns4yI="
      "bbigras-nix-config.cachix.org-1:aXL6Q9Oi0jyF79YAKRu17iVNk9HY0p23OZX7FA8ulhU="
      "mjlbach.cachix.org-1:dR0V90mvaPbXuYria5mXvnDtFibKYqYc2gtl9MWSkqI="
      "pre-commit-hooks.cachix.org-1:Pkk3Panw5AW24TOv6kz3PvLhlH8puAsJTBbOPmBo7Rc="
      "bbigras-dev.cachix.org-1:7JrfmXK/0cXRCzguretr0wlWtY2zp/mkp/HwMwIJzCw="
      "cache.ngi0.nixos.org-1:KqH5CBLNSyX184S9BKZJo1LxrxJ9ltnY2uAs5c/f1MA="
      "nix-on-droid.cachix.org-1:56snoMJTXmDRC1Ei24CmKoUqvHJ9XCp+nidK7qkMQrU="
    ];
  };
}
