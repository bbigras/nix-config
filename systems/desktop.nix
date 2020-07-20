# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [
      # (import ../nix).impermanence-sys

      ../core
      # ../dev

      (import ../nix).cpu_intel
      (import ../nix).ssd

      # Include the results of the hardware scan.
      ../hardware/hardware-configuration-desktop.nix
      ../hardware/efi.nix

      ../dev/qemu.nix
      ../dev/virt-manager.nix

      # ../configuration_common.nix
      # ./aarch64.nix

      ../gnome.nix

      ../users/bbigras
    ];

  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.loader.grub.useOSProber = true;
  # hardware.enableRedistributableFirmware = true;
  networking.hostName = "desktop"; # Define your hostname.
  programs.thefuck.enable = true;
  time.hardwareClockInLocalTime = true;
  users.users.bbigras.packages = [ pkgs.retroarchBare ];

  services.xserver = {
    autorun = true;
    displayManager.hiddenUsers = [ "builder" ];
    enable = true;
    videoDrivers = [ "nvidia" ];
  };

  boot.kernel.sysctl = {
    "kernel.sysrq" = 1;
    # "fs.inotify.max_user_watches" = 524288;
    # "vm.swappiness" = 1;
  };

  fileSystems."/media/gamedisk" =
    {
      device = "/dev/disk/by-uuid/A238EB6A38EB3BC3";
      fsType = "ntfs";
      options = [ "uid=1000,gid=100,rw,user,exec,umask=000" ];
    };

  # networking.firewall.enable = false;
  networking.firewall.allowedTCPPorts = [
    9977
    9988
  ];
  #   22000
  #   6680
  #   51413 # transmission
  #   19515 # qbittorrent
  # ];
  # networking.firewall.allowedTCPPortRanges = [
  #   { from = 6881; to = 6999; } # aria2c
  # ];
  # networking.firewall.allowedUDPPorts = [
  #   21027
  # ];
  # networking.firewall.allowedUDPPortRanges = [
  #   { from = 6881; to = 6999; } # aria2c
  # ];

  users.users.builder = {
    createHome = true;
    isNormalUser = true;
  };

  # services.resilio = {
  #   enable = true;
  #   # openFirewall = true;
  #   enableWebUI = true;
  # };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.09"; # Did you read the comment?

}
