{
  hostType,
  lib,
  pkgs,
  ...
}:
{
  nix = {
    package = pkgs.nixVersions.nix_2_31;
    settings = {
      accept-flake-config = true;
      # XXX: Causes annoying "cannot link ... to ...: File exists" errors on Darwin
      auto-optimise-store = hostType == "nixos";
      allowed-users = [ "@wheel" ];
      build-users-group = "nixbld";
      builders-use-substitutes = true;
      trusted-users = [
        "root"
        "@wheel"
      ];
      sandbox = hostType == "nixos";
      substituters = [
        "http://192.168.68.6:8501?priority=1"
        "https://bbigras-nix-config.cachix.org"
        "https://nix-community.cachix.org"
      ];
      trusted-public-keys = [
        "192.168.68.6:zSAiwQJTX02yGP2NSof1Pin339R5YP+91Y5xdaqFsnU="
        "bbigras-nix-config.cachix.org-1:aXL6Q9Oi0jyF79YAKRu17iVNk9HY0p23OZX7FA8ulhU="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
      cores = 0;
      max-jobs = "auto";
      experimental-features = [
        "auto-allocate-uids"
        "configurable-impure-env"
        "flakes"
        "nix-command"
      ];
      connect-timeout = 5;
      http-connections = 0;
      # flake-registry = "/etc/nix/registry.json";
      always-allow-substitutes = true;
    };

    distributedBuilds = true;
    extraOptions = ''
      !include tokens.conf
    '';
  }
  // lib.optionalAttrs (hostType == "nixos") {
    channel.enable = false;
    # daemonCPUSchedPolicy = "batch";
    daemonIOSchedPriority = 5;
    nixPath = [ "nixpkgs=/run/current-system/nixpkgs" ];
    optimise = {
      automatic = false;
      dates = [ "03:00" ];
    };
  }
  // lib.optionalAttrs (hostType == "darwin") {
    nixPath = [ "nixpkgs=/run/current-system/sw/nixpkgs" ];
    daemonIOLowPriority = false;
  };
}
