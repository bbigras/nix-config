{
  config,
  pkgs,
  nur,
  nixos-hardware,
  ...
}:

let
  nurNoPkgs = import nur {
    pkgs = null;
    nurpkgs = pkgs;
  };
in
{
  imports =
    with nixos-hardware.nixosModules;
    [
      ../../core
      ../../dev
      # ../../dev/virt-manager.nix
      ../../dev/incus.nix

      # Include the results of the hardware scan.
      ../../hardware/hardware-configuration-work.nix
      ../../hardware/efi.nix

      common-pc
      common-pc-ssd
      common-cpu-intel

      ../../users/bbigras
    ]
    ++ (
      if builtins.pathExists (builtins.getEnv "PWD" + "/secrets/at_work.nix") then
        [ (builtins.getEnv "PWD" + "/secrets/at_work.nix") ]
      else
        [ ]
    );

  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernel.sysctl = {
    "kernel.sysrq" = 1;
    # "fs.inotify.max_user_watches" = 524288;
    # "vm.swappiness" = 1;
  };

  virtualisation.docker.enable = true;

  environment.systemPackages = with pkgs; [ linuxPackages_zen.bcc ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/ccb4db0b-1cb8-4d5f-be9a-8b20a5c63982";
    fsType = "btrfs";
    options = [
      "subvol=nixos"
      "compress=zstd"
    ];
  };

  nix = {
    settings = {
      trusted-users = [
        "hydra-queue-runner"
        "hydra"
        "hydra-www"
      ];

      substituters = [
        "https://cache.nixos.org"
        "https://nix-community.cachix.org"
      ];
      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };

    extraOptions = ''
      builders-use-substitutes = true
      netrc-file = ${config.sops.secrets.netrc.path};
    '';
  };

  services.hydra = {
    enable = true;
    hydraURL = "http://localhost:3000";
    buildMachinesFiles = [ ];
    useSubstitutes = true;
    notificationSender = "fake@test.local";
  };

  services.smartd.enable = true;

  sops.secrets = {
    netrc.sopsFile = ./secrets.yaml;
  };

  networking = {
    hostName = "bbigras-work";
  };

  home-manager.users.bbigras = {
    imports = [
      ../../users/bbigras/trusted
      nurNoPkgs.repos.rycee.hmModules.emacs-init
    ];
  };
}
