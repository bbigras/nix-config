{
  nix = {
    settings = {
      allowed-users = [ "@wheel" ];
      trusted-users = [ "root" "@wheel" ];
      system-features = [ "recursive-nix" ];
    };
    daemonCPUSchedPolicy = "batch";
    daemonIOSchedPriority = 5;
    distributedBuilds = true;
    extraOptions = ''
      builders-use-substitutes = true
      experimental-features = nix-command flakes recursive-nix
      flake-registry = /etc/nix/registry.json
    '';
    # nrBuildUsers = config.nix.settings.max-jobs * 2;
    optimise = {
      automatic = true;
      dates = [ "03:00" ];
    };
  };
}
