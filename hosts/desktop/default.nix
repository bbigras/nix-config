# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, nur, nixos-hardware, ... }:

let
  qemu-aarch64-static = pkgs.stdenv.mkDerivation {
    name = "qemu-aarch64-static";

    src = builtins.fetchurl {
      url = "https://github.com/multiarch/qemu-user-static/releases/download/v6.1.0-6/qemu-aarch64-static";
      sha256 = "0c41s036iazrxn2ff86cvry51kj4fc0ygi7pway4z42ax0f46vm4";
    };

    dontUnpack = true;
    installPhase = "install -D -m 0755 $src $out/bin/qemu-aarch64-static";
  };

  nurNoPkgs = import nur { pkgs = null; nurpkgs = pkgs; };
in
{
  imports = with nixos-hardware.nixosModules;
    [
      ../../core
      ../../services/veilid.nix
      ../../dev
      ../../dev/virt-manager.nix

      # Include the results of the hardware scan.
      ../../hardware/hardware-configuration-desktop.nix
      ../../hardware/efi.nix
      #../../hardware/secureboot.nix

      common-pc
      common-pc-ssd
      common-cpu-intel-cpu-only
      common-gpu-amd

      ../../hardware/sound.nix

      # ./aarch64.nix

      ../../graphical
      ../../graphical/gnome.nix
      ../../graphical/trusted.nix

      # ../../dev/rust-embeded.nix
      ../../dev/adb.nix

      ../../users/bbigras
      # ./hass-podman.nix
    ] ++ (if builtins.pathExists (builtins.getEnv "PWD" + "/secrets/at_home.nix") then [ (builtins.getEnv "PWD" + "/secrets/at_home.nix") ] else [ ])
    ++ (if builtins.pathExists (builtins.getEnv "PWD" + "/secrets/desktop.nix") then [ (builtins.getEnv "PWD" + "/secrets/desktop.nix") ] else [ ]);

  catppuccin.flavor = "mocha";
  catppuccin.accent = "blue";

  nix = {
    extraOptions = ''
      extra-platforms = aarch64-linux i686-linux
      trusted-users = bbigras
    '';
    settings = {
      extra-sandbox-paths = [ "/run/binfmt/aarch64=${qemu-aarch64-static}/bin/qemu-aarch64-static" ];
      system-features = [ "benchmark" "nixos-test" "big-parallel" "kvm" ];
    };
  };

  boot.kernelPackages = pkgs.linuxPackages_latest;

  boot.plymouth = {
    enable = false;
    catppuccin.enable = true;
  };

  networking.interfaces."enp6s0".wakeOnLan.enable = true;
  # boot.extraModulePackages = [ config.boot.kernelPackages.v4l2loopback ];

  boot = {
    extraModulePackages = with config.boot.kernelPackages; [
      # v4l2loopback.out
    ];
    kernelModules = [
      # "v4l2loopback"
    ];
    extraModprobeConfig = ''
      # options v4l2loopback exclisive_caps=1 card_label="Virtual Camera"
    '';
  };

  boot = {
    binfmt.registrations.aarch64 = {
      interpreter = "${qemu-aarch64-static}/bin/qemu-aarch64-static";
      magicOrExtension = ''\x7fELF\x02\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\xb7\x00'';
      mask = ''\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\x00\xff\xfe\xff\xff\xff'';
    };
    loader.grub.useOSProber = true;

    loader.systemd-boot.memtest86.enable = true;

    kernel.sysctl = {
      "kernel.sysrq" = 1;
      # "fs.inotify.max_user_watches" = 524288;
      # "vm.swappiness" = 1;
    };
  };

  home-manager.users.bbigras = {
    imports = [
      ../../users/bbigras/trusted
      nurNoPkgs.repos.rycee.hmModules.emacs-init
    ];

    xdg.mimeApps.enable = lib.mkForce false;
  };

  sops.secrets = {
    wireguard.sopsFile = ./restic-desktop.yaml;
  };

  powerManagement.cpuFreqGovernor = "performance";

  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
    extraPackages = with pkgs; [
      libva
      vaapiVdpau
      libvdpau-va-gl
      amdvlk
    ];
    extraPackages32 = with pkgs; [
      driversi686Linux.amdvlk
    ];
  };
  boot.initrd.kernelModules = [ "amdgpu" ];


  # hardware.enableRedistributableFirmware = true;
  networking.hostName = "desktop"; # Define your hostname.
  networking.networkmanager.enable = false;
  users.users.bbigras.packages = [ pkgs.retroarchBare ];

  services.smartd.enable = true;

  programs.steam.enable = true;
  programs.steam.remotePlay.openFirewall = true;
  programs.gamemode.enable = true;
  systemd.network = {
    enable = true;
    networks = {
      "10-lan" = {
        DHCP = "yes";
        matchConfig.Name = "enp*";
        # domains = [ "~." ];
        dhcpV4Config = { UseDNS = false; };
        dns = [
          # https://developers.cloudflare.com/1.1.1.1/dns-over-tls
          "1.1.1.1#cloudflare-dns.com"
          "1.0.0.1#cloudflare-dns.com"
        ];
        networkConfig = {
          # DNSSEC = true;
          DNSOverTLS = true;
        };
      };
    };
  };

  services.xserver = {
    autorun = true;
    enable = true;
  };

  boot.supportedFilesystems = [ "ntfs" ];

  # networking.firewall.enable = false;
  networking.firewall.allowedTCPPorts = [
    9977
    9988
    8096 # jellyfin
    21000 # immersed
    21013 # immersed
    8077 # qBittorrent
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

  environment.systemPackages = with pkgs; [
    fwupd
  ];

  virtualisation.docker.enable = true;

  services.fwupd.enable = true;

  services.flatpak.enable = true;

  services.jellyfin = {
    enable = true;
    openFirewall = true;
  };

  services.earlyoom.enable = true;
}
