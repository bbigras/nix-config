{
  agenix,
  config,
  disko,
  lib,
  pkgs,
  home-manager,
  impermanence,
  lanzaboote,
  nix-index-database,
  nixos-facter-modules,
  catppuccin,
  sops-nix,
  nixpkgs-veilid,
  ...
}:
{
  imports = [
    disko.nixosModules.disko
    home-manager.nixosModules.home-manager
    impermanence.nixosModules.impermanence
    lanzaboote.nixosModules.lanzaboote
    nix-index-database.nixosModules.nix-index
    nixos-facter-modules.nixosModules.facter
    sops-nix.nixosModules.sops
    catppuccin.nixosModules.catppuccin
    ./openssh.nix
    ./resolved.nix
    ./tailscale.nix
    ./tmux.nix
    {
      imports = [ (nixpkgs-veilid + "/nixos/modules/services/networking/veilid.nix") ];
      disabledModules = [ "services/networking/veilid.nix" ];
    }
  ];

  boot.kernelParams = [ "log_buf_len=10M" ];
  services.systembus-notify.enable = true;

  documentation = {
    dev.enable = true;
    man.generateCaches = true;
  };

  environment.systemPackages = with pkgs; [
    ghostty.terminfo
    nix-output-monitor
  ];

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
    localsend.enable = true;
    mosh.enable = true;
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
    systemBuilderCommands = ''
      ln -sv ${pkgs.path} $out/nixpkgs
    '';
    rebuild.enableNg = true;
    stateVersion = "23.05";
  };

  time.timeZone = "America/Montreal";

  systemd = {
    network.wait-online.anyInterface = true;
  };

  users.mutableUsers = false;
}
