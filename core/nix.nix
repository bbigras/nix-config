{ config, pkgs, ... }: {
  nix = {
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
    settings = {
      system-features = [ "recursive-nix" ];
      trusted-users = [ "root" "@wheel" ];
      allowed-users = [ "@wheel" ];
    };
  };
}
