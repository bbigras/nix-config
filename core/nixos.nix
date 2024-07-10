{ agenix, config, lib, pkgs, home-manager, impermanence, lanzaboote, nix-index-database, catppuccin, sops-nix, ... }:
{
  imports = [
    home-manager.nixosModules.home-manager
    impermanence.nixosModules.impermanence
    lanzaboote.nixosModules.lanzaboote
    nix-index-database.nixosModules.nix-index
    sops-nix.nixosModules.sops
    catppuccin.nixosModules.catppuccin
    ./openssh.nix
    ./podman.nix
    ./resolved.nix
    ./solo2.nix
    ./tailscale.nix
    ./tmux.nix
    ./zsh.nix
  ];

  boot.kernelParams = [ "log_buf_len=10M" ];
  services.systembus-notify.enable = true;

  documentation = {
    dev.enable = true;
    man.generateCaches = true;
  };

  i18n.defaultLocale = "fr_CA.UTF-8";

  networking = {
    firewall = {
      allowedUDPPorts = [
        22000 # syncthing
        21027 # syncthing discovery
      ];
      allowedTCPPorts = [
        22000 # syncthing
      ];
    };
    useDHCP = false;
    useNetworkd = true;
    wireguard.enable = true;
  };

  programs = {
    command-not-found.enable = false;
    mosh.enable = true;
    zsh.enableGlobalCompInit = false;
  };

  security = {
    # pam.services.sudo.u2fAuth = true;
    sudo = {
      enable = true;
      wheelNeedsPassword = lib.mkDefault false;
    };
  };

  services = {
    dbus.implementation = "broker";
    openssh = {
      enable = true;
      settings.PermitRootLogin = lib.mkDefault "no";
    };
    fwupd.daemonSettings.EspLocation = config.boot.loader.efi.efiSysMountPoint;
  };

  system = {
    extraSystemBuilderCmds = ''
      ln -sv ${pkgs.path} $out/nixpkgs
    '';

    stateVersion = "22.11";
  };

  time.timeZone = "America/Montreal";

  systemd = {
    enableUnifiedCgroupHierarchy = true;
    network.wait-online.anyInterface = true;
  };

  users.mutableUsers = false;
}
