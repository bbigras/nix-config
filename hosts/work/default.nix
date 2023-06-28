{ lib, pkgs, nur, nixos-hardware, attic, ... }:

let
  nurNoPkgs = import nur { pkgs = null; nurpkgs = pkgs; };

  my_attic = (import attic);

  upload_to_attic = pkgs.writeScriptBin "upload-to-attic" ''
    #!/bin/sh
    set -eu
    set -f # disable globbing

    # skip push if the declarative job spec
    OUT_END=$(echo ''${OUT_PATHS: -10})
    if [ "$OUT_END" == "-spec.json" ]; then
    exit 0
    fi

    export HOME=/root
    exec ${my_attic}/bin/attic push luxor $OUT_PATHS > /tmp/hydra_attic 2>&1
  '';
in
{
  imports = with nixos-hardware.nixosModules;
    [
      ../../core
      ../../dev

      # Include the results of the hardware scan.
      ../../hardware/hardware-configuration-work.nix
      ../../hardware/efi.nix
      ../../hardware/sound.nix

      common-pc
      common-pc-ssd
      common-cpu-intel

      ../../graphical
      ../../graphical/sway.nix
      ../../graphical/trusted.nix
      ../../dev/virt-manager.nix

      ../../users/bbigras
    ] ++ (if builtins.pathExists (builtins.getEnv "PWD" + "/secrets/at_work.nix") then [ (builtins.getEnv "PWD" + "/secrets/at_work.nix") ] else [ ]);

  nix = {
    # extraOptions = ''
    #   extra-platforms = i686-linux
    #   trusted-users = bbigras
    # '';
    # settings = {
    #   system-features = [ "benchmark" "nixos-test" "big-parallel" "kvm" ];
    # };
  };

  boot.kernelPackages = pkgs.linuxPackages_zen;
  boot.kernel.sysctl = {
    "kernel.sysrq" = 1;
    # "fs.inotify.max_user_watches" = 524288;
    # "vm.swappiness" = 1;
  };

  virtualisation.docker.enable = true;

  environment.systemPackages = with pkgs; [ linuxPackages_zen.bcc ];

  fileSystems."/" =
    {
      device = "/dev/disk/by-uuid/ccb4db0b-1cb8-4d5f-be9a-8b20a5c63982";
      fsType = "btrfs";
      options = [ "subvol=nixos" "compress=zstd" ];
    };

  services.printing = {
    enable = true;
    drivers = with pkgs; [ postscript-lexmark hplip ];
  };

  services.yggdrasil.enable = lib.mkForce false;

  nix = {
    settings = {
      trusted-users = [ "hydra-queue-runner" "hydra" ];
      # trusted-users = [ "root" "@wheel" "hydra-queue-runner" "hydra" ];
      # allowed-users = [ "root" "@wheel" "hydra-queue-runner" ];

      substituters = [
        "https://cache.nixos.org"
        "https://nix-community.cachix.org"
        "https://attic.tech.luxorcollection.com/luxor"
      ];
      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "luxor:z/e2l6p5oK8iS09ei9hYsyWngcjPrhLEzBfnUW52/js="
      ];

    };

    extraOptions = ''
      builders-use-substitutes = true
      post-build-hook = ${upload_to_attic}/bin/upload-to-attic
      netrc-file = /opt/netrc
    '';

  };

  services.hydra = {
    enable = true;
    hydraURL = "http://localhost:3000";
    notificationSender = "hydra@luxorcollection.com";
    buildMachinesFiles = [ ];
    useSubstitutes = true;
    # smtpHost
    # notificationSender
    # extraConfig = ''
    # <gitea_authorization>
    #   bbigras=ec6e0df40e270ddb85a161b9bd98e83192ea6296
    # </gitea_authorization>
    # '';
  };

  networking = {
    hostName = "bbigras-work";
  };

  home-manager.users.bbigras = {
    imports = [
      ../../users/bbigras/trusted
      nurNoPkgs.repos.rycee.hmModules.emacs-init
    ];

    wayland.windowManager.sway = {
      config = {
        input = {
          "1118:1974:Microsoft_Comfort_Curve_Keyboard_3000" = {
            xkb_numlock = "enabled";
          };
        };
      };
    };
  };
}
