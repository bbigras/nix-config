# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:
let
  zen = (import (import ../nix).zen { });
in
{
  imports =
    [
      ../core

      # Include the results of the hardware scan.
      ../hardware/hardware-configuration-work.nix
      ../hardware/efi.nix

      (import ../nix).cpu_intel
      (import ../nix).ssd

      ../sway
      ../sway/trusted.nix

      ../users/bbigras
    ] ++ (if builtins.pathExists ../secrets/at_work.nix then [ ../secrets/at_work.nix ] else [ ]);

  boot.kernelPackages = zen.linuxPackages_zen;
  boot.kernel.sysctl = {
    "kernel.sysrq" = 1;
    # "fs.inotify.max_user_watches" = 524288;
    # "vm.swappiness" = 1;
  };

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

  environment.etc."restic-pw-id".text = ''
    BW_ID=zzz
  '';

  home-manager.users.bbigras = {
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

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?
}
