{
  config,
  pkgs,
  nur,
  ...
}:

let
  nurNoPkgs = import nur {
    pkgs = null;
    nurpkgs = pkgs;
  };
in
{
  imports = [
    ../../core
    ../../dev
    # ../../dev/virt-manager.nix
    ../../dev/incus.nix
    ../../hardware/efi.nix
    ../../users/bbigras
    { config.facter.reportPath = ./facter.json; }
  ]
  ++ (
    if builtins.pathExists (builtins.getEnv "PWD" + "/secrets/at_work.nix") then
      [ (builtins.getEnv "PWD" + "/secrets/at_work.nix") ]
    else
      [ ]
  );

  boot.kernel.sysctl = {
    "kernel.sysrq" = 1;
    # "fs.inotify.max_user_watches" = 524288;
    # "vm.swappiness" = 1;
  };

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
