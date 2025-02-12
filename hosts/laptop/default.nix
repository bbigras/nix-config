# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
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
      ./broadcom-wifi.nix

      ../../core
      ../../dev
      # ../../dev/virt-manager.nix
      ../../services/veilid.nix

      # Include the results of the hardware scan.
      ../../hardware/hardware-configuration-laptop.nix
      ../../hardware/efi.nix
      ../../hardware/bluetooth.nix
      ../../hardware/sound.nix
      dell-xps-13-9343
      common-hidpi

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
      if builtins.pathExists (builtins.getEnv "PWD" + "/secrets/laptop.nix") then
        [ (builtins.getEnv "PWD" + "/secrets/laptop.nix") ]
      else
        [ ]
    );

  environment.systemPackages = with pkgs; [
    iwd
  ];

  sops.secrets = {
    restic-laptop-password.sopsFile = ./restic-laptop.yaml;
    restic-laptop-creds.sopsFile = ./restic-laptop.yaml;
    wireguard.sopsFile = ./restic-laptop.yaml;
  };

  # boot.kernelPackages = pkgs.linuxPackages_latest;

  catppuccin = {
    enable = true;
    flavor = "mocha";
    accent = "blue";
  };

  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [ ];

  hardware.brillo.enable = true;
  boot.kernel.sysctl = {
    "kernel.sysrq" = 1;
    # "fs.inotify.max_user_watches" = 524288;
    # "vm.swappiness" = 1;
  };
  swapDevices = [
    {
      device = "/swapfile";
    }
  ];

  boot.initrd.availableKernelModules = [
    "aesni_intel"
    "cryptd"
  ];

  networking = {
    useNetworkd = true;
    hostName = "laptop"; # Define your hostname.
    wireless = {
      enable = false;
      iwd = {
        enable = true;
        settings = {
          Settings = {
            AutoConnect = true;
          };
        };
      };
    };
  };

  networking.networkmanager.enable = false;

  systemd.network = {
    enable = true;
    networks = {
      "10-wifi" = {
        DHCP = "yes";
        matchConfig.Name = "eth0";
        dhcpV4Config = {
          UseDNS = false;
        };
      };
    };
  };

  services.flatpak.enable = true;
  services.smartd.enable = true;
  services.irqbalance.enable = true;

  services.earlyoom = {
    enable = true;
    enableNotifications = true;
  };
  services = {
    fwupd.enable = false;
    thermald.enable = true;
  };
  services.desktopManager.cosmic.enable = true;
  services.displayManager.cosmic-greeter.enable = true;

  zramSwap = {
    enable = true;
    algorithm = "zstd";
  };

  home-manager.users.bbigras = {
    imports = [
      ../../users/bbigras/trusted
      nurNoPkgs.repos.rycee.hmModules.emacs-init
    ];

    services.gnome-keyring = {
      enable = true;
      components = [ "secrets" ];
    };
  };
}
