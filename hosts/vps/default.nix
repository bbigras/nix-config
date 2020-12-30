{ pkgs, modulesPath, ... }:

{
  imports =
    [
      (modulesPath + "/virtualisation/digital-ocean-config.nix")
    ];

  networking.hostName = "vps";

  boot.cleanTmpDir = true;

  environment.systemPackages = with pkgs; [
    git
    nixpkgs-fmt
    tmux
  ];

  nix.trustedUsers = [ "root" "bbigras" ];
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };

  users.users.bbigras = {
    createHome = true;
    isNormalUser = true;
    extraGroups = [
      "wheel"
      "docker"
    ];
  };

  virtualisation.oci-containers = {
    # backend = "podman";
  };

  services.openssh.openFirewall = false;
}
