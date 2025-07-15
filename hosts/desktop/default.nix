# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{
  config,
  lib,
  pkgs,
  nur,
  ...
}:

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

  nurNoPkgs = import nur {
    pkgs = null;
    nurpkgs = pkgs;
  };
  json = pkgs.formats.json { };
in
{
  imports = [
    ../../core
    ../../services/kanata.nix
    ../../services/podman.nix
    ../../services/veilid.nix
    ../../services/virt-manager.nix
    ../../dev
    ../../dev/incus.nix

    # Include the results of the hardware scan.
    ../../hardware/efi.nix
    #../../hardware/secureboot.nix

    { config.facter.reportPath = ./facter.json; }

    ../../hardware/sound.nix

    # ./aarch64.nix

    ../../graphical
    ../../graphical/trusted.nix

    # ../../dev/rust-embeded.nix
    ../../dev/adb.nix

    ../../users/bbigras
  ]
  ++ (
    if builtins.pathExists (builtins.getEnv "PWD" + "/secrets/at_home.nix") then
      [ (builtins.getEnv "PWD" + "/secrets/at_home.nix") ]
    else
      [ ]
  )
  ++ (
    if builtins.pathExists (builtins.getEnv "PWD" + "/secrets/desktop.nix") then
      [ (builtins.getEnv "PWD" + "/secrets/desktop.nix") ]
    else
      [ ]
  );

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/e58653d8-7f76-402d-998d-400fe04f7520";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/DA5A-BC65";
    fsType = "vfat";
    options = [
      "fmask=0022"
      "dmask=0022"
    ];
  };

  fileSystems."/media/gamedisk" = {
    device = "/dev/disk/by-uuid/CA909C6D909C622D";
    fsType = "ntfs3";
  };

  catppuccin = {
    enable = true;
    flavor = "mocha";
    accent = "blue";
  };

  nix = {
    extraOptions = ''
      extra-platforms = aarch64-linux i686-linux
    '';
    settings = {
      substituters = [
        "http://192.168.68.6:8501?priority=1"
      ];
      trusted-public-keys = [
        "192.168.68.6:zSAiwQJTX02yGP2NSof1Pin339R5YP+91Y5xdaqFsnU="
      ];
      extra-sandbox-paths = [ "/run/binfmt/aarch64=${qemu-aarch64-static}/bin/qemu-aarch64-static" ];
      system-features = [
        "benchmark"
        "nixos-test"
        "big-parallel"
        "kvm"
      ];
    };
  };

  #boot.kernelPackages = pkgs.linuxPackages_latest;

  boot.plymouth.enable = true;

  # networking.interfaces."enp6s0".wakeOnLan.enable = true;

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
  };

  sops.secrets = {
    wireguard.sopsFile = ./restic-desktop.yaml;
  };

  powerManagement.cpuFreqGovernor = "performance";

  # hardware.enableRedistributableFirmware = true;
  networking.hostName = "desktop"; # Define your hostname.
  networking.networkmanager.enable = false;

  services.smartd.enable = true;
  services.gnome.gnome-keyring.enable = false;

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
        dhcpV4Config = {
          UseDNS = false;
        };
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
  # ];
  # networking.firewall.allowedUDPPorts = [
  #   21027
  # ];
  # networking.firewall.allowedUDPPortRanges = [
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

  services.ollama = {
    enable = false;
    acceleration = "rocm";
    environmentVariables = {
      HCC_AMDGPU_TARGET = "gfx1031"; # used to be necessary, but doesn't seem to anymore
    };
    rocmOverrideGfx = "10.3.1";
  };

  services.open-webui = {
    enable = false;
    host = "0.0.0.0";
  };

  environment.systemPackages = with pkgs; [
    fwupd
  ];

  services.fwupd.enable = true;

  services.flatpak.enable = true;

  services.earlyoom.enable = false;
  services.desktopManager.cosmic.enable = true;
  services.displayManager.cosmic-greeter.enable = true;
  environment.sessionVariables.COSMIC_DATA_CONTROL_ENABLED = 1;
}
