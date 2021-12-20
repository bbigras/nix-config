{ config, pkgs, ... }: {
  nix = {
    allowedUsers = [ "@wheel" ];
    daemonCPUSchedPolicy = "batch";
    daemonIOSchedPriority = 5;
    distributedBuilds = true;
    extraOptions = ''
      builders-use-substitutes = true
      experimental-features = nix-command flakes recursive-nix
    '';
    # nrBuildUsers = config.nix.maxJobs * 2;
    optimise = {
      automatic = false;
      dates = [ "03:00" ];
    };
    package = pkgs.nixUnstable;
    systemFeatures = [ "recursive-nix" ];
    trustedUsers = [ "root" "@wheel" ];
  };
}
