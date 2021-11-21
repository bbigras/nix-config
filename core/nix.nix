{ config, ... }: {
  nix = {
    allowedUsers = [ "@wheel" ];
    binaryCaches = [
      "https://nix-community.cachix.org"
    ];
    binaryCachePublicKeys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
    daemonCPUSchedPolicy = "batch";
    daemonIOSchedPriority = 5;
    distributedBuilds = true;
    extraOptions = ''
      builders-use-substitutes = true
      experimental-features = nix-command flakes recursive-nix
      flake-registry = /etc/nix/registry.json
    '';
    # nrBuildUsers = config.nix.maxJobs * 2;
    optimise = {
      automatic = false;
      dates = [ "03:00" ];
    };
    systemFeatures = [ "recursive-nix" ];
    trustedUsers = [ "root" "@wheel" ];
  };
}
