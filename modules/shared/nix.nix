# Shared nix settings for NixOS and Darwin
{ lib, pkgs, ... }:
let
  inherit (pkgs.stdenv) isDarwin;
in
{
  nix = lib.mkMerge [
    {
      package = pkgs.nixVersions.latest;
      settings = {
        accept-flake-config = true;
        allowed-users = [ "@wheel" ];
        build-users-group = "nixbld";
        builders-use-substitutes = true;
        trusted-users = [
          "root"
          "@wheel"
        ];
        substituters = [
          "http://192.168.0.201:8501?priority=1"
          "https://bbigras-nix-config.cachix.org"
          "https://nix-community.cachix.org"
        ];
        trusted-public-keys = [
          "192.168.0.201:zSAiwQJTX02yGP2NSof1Pin339R5YP+91Y5xdaqFsnU="
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

    # NixOS-specific (use optionalAttrs to avoid defining non-existent options)
    (lib.optionalAttrs (!isDarwin) {
      settings = {
        auto-optimise-store = true;
        sandbox = true;
      };
      channel.enable = false;
      daemonCPUSchedPolicy = "batch";
      daemonIOSchedPriority = 5;
      optimise = {
        automatic = true;
        dates = [ "03:00" ];
      };
    })

    # Darwin-specific (use optionalAttrs to avoid defining non-existent options)
    (lib.optionalAttrs isDarwin {
      settings = {
        # Causes annoying "cannot link ... to ...: File exists" errors on Darwin
        auto-optimise-store = false;
        sandbox = false;
      };
      daemonIOLowPriority = false;
    })
  ];
}
