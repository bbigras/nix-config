{
  agenix,
  config,
  lib,
  pkgs,
  home-manager,
  impermanence,
  lanzaboote,
  nix-index-database,
  nixos-facter-modules,
  catppuccin,
  sops-nix,
  ...
}:
{
  imports = [
    home-manager.nixosModules.home-manager
    impermanence.nixosModules.impermanence
    lanzaboote.nixosModules.lanzaboote
    nix-index-database.nixosModules.nix-index
    nixos-facter-modules.nixosModules.facter
    sops-nix.nixosModules.sops
    catppuccin.nixosModules.catppuccin
    ./netbird.nix
    ./openssh.nix
    ./podman.nix
    ./resolved.nix
    ./tailscale.nix
    ./tmux.nix
  ];

  boot.initrd.systemd.enable = true;
  boot.kernelParams = [ "log_buf_len=10M" ];
  services.systembus-notify.enable = true;

  documentation = {
    dev.enable = true;
    man.generateCaches = true;
  };

  i18n.defaultLocale = "fr_CA.UTF-8";
  environment.systemPackages = with pkgs; [
    ghostty.terminfo
    nix-output-monitor
  ];

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
    extraSystemBuilderCmds = ''
      ln -sv ${pkgs.path} $out/nixpkgs
    '';
    rebuild.enableNg = true;
    switch.enableNg = true;
    stateVersion = "23.05";
  };

  time.timeZone = "America/Montreal";

  systemd = {
    network.wait-online.anyInterface = true;
  };

  users.mutableUsers = false;
}
