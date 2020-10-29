{ pkgs, ... }:
let
  dummyConfig = pkgs.writeText "configuration.nix" ''
    assert builtins.trace "This is a dummy config, use nixus!" false;
    {}
  '';
in
{
  imports = [
    (import (import ../nix).home-manager)
    ./adb.nix
    ./docker.nix
    ./nix.nix
    ./openssh.nix
    ./steam.nix
    ./sudo.nix
    ./tailscale.nix
    ./zerotier.nix
  ];

  nix.extraOptions = ''
    keep-outputs = true
    keep-derivations = true
  '';

  # boot.kernelPackages = pkgs.linuxPackages_latest;
  # boot.extraModulePackages = [ config.boot.kernelPackages.exfat-nofuse ];
  environment.etc."nixos/configuration.nix".source = dummyConfig;
  environment.systemPackages = with pkgs; [ ntfs3g ];
  home-manager.useGlobalPkgs = true;
  i18n.defaultLocale = "fr_CA.UTF-8";
  nix.maxJobs = "auto";
  nixpkgs.localSystem.system = "x86_64-linux";

  networking.useDHCP = false;

  programs.fish.enable = true;
  programs.ssh.startAgent = true;
  programs.wireshark.enable = true;
  services.fwupd.enable = true; # TODO: check if needed
  users.mutableUsers = false;

  programs.gnupg.agent = {
    enable = true;
    #   enableSSHSupport = true;
    pinentryFlavor = "gnome3";
  };

  nix = {
    binaryCaches = [
      "https://nix-community.cachix.org"
      "https://nix-matrix-yggdrasil.cachix.org"
      "https://eigenvalue.cachix.org" # for crate2nix
      "https://bbigras-nix-config.cachix.org"
      "https://srid.cachix.org" # for neuron
      "https://nixiosk.cachix.org"
      "https://niv.cachix.org"
      "https://mjlbach.cachix.org"
      "https://pre-commit-hooks.cachix.org"
    ];
    binaryCachePublicKeys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "nix-matrix-yggdrasil.cachix.org-1:zxDWWJFEiOxpvKHP8qoi2HS4CtHxUPPQtoWz9D66m9g="
      "eigenvalue.cachix.org-1:ykerQDDa55PGxU25CETy9wF6uVDpadGGXYrFNJA3TUs="
      "bbigras-nix-config.cachix.org-1:aXL6Q9Oi0jyF79YAKRu17iVNk9HY0p23OZX7FA8ulhU="
      "srid.cachix.org-1:MTQ6ksbfz3LBMmjyPh0PLmos+1x+CdtJxA/J2W+PQxI="
      "nixiosk.cachix.org-1:pyzRzjCUhw0r+moXnSklZwwI/gFk+Z+A2ofmEhOf7Sc="
      "niv.cachix.org-1:X32PCg2e/zAm3/uD1ScqW2z/K0LtDyNV7RdaxIuLgQM="
      "mjlbach.cachix.org-1:dR0V90mvaPbXuYria5mXvnDtFibKYqYc2gtl9MWSkqI="
      "pre-commit-hooks.cachix.org-1:Pkk3Panw5AW24TOv6kz3PvLhlH8puAsJTBbOPmBo7Rc="
    ];
    nixPath = [
      "nixos-config=${dummyConfig}"
      "nixpkgs=/run/current-system/nixpkgs"
      "nixpkgs-overlays=/run/current-system/overlays"
    ];
  };

  nixpkgs = {
    config.allowUnfree = true;
    overlays = [
      (import (import ../nix).nixpkgs-mozilla)
      (import (import ../nix).emacs-overlay)
      (import (import ../nix).emacs-pgtk-nativecomp-overlay)
      (import ../overlays/mkSecret.nix)
    ];
  };

  services.yggdrasil = {
    enable = true;
    openMulticastPort = true;
    config = {
      Listen = [ "tcp://0.0.0.0:9977" ];
      Peers = [
        "tcp://64.112.177.94:1617"
        "tcp://64.112.180.77:1617"
        "tcp://50.236.201.218:56088"
      ];
      LinkLocalTCPPort = 9988;
      denyDhcpcdInterfaces = [ "tap*" ];
      SessionFirewall = {
        Enable = true;
        AllowFromDirect = false;
        AllowFromRemote = false;
      };
    };
  };

  # networking.resolvconf.useLocalResolver ?

  services.resolved = {
    enable = true;
    dnssec = "false";
    # dnssec = "allow-downgrade";
    # DNSOverTLS=yes
    llmnr = "false";

    extraConfig = ''
      DNS=1.1.1.1#cloudflare-dns.com 1.0.0.1#cloudflare-dns.com
      DNSOverTLS=true
    '';
  };

  # services = {
  #   dbus.socketActivated = true;
  # };

  system = {
    extraSystemBuilderCmds = ''
      ln -sv ${pkgs.path} $out/nixpkgs
      ln -sv ${../overlays} $out/overlays
    '';

    stateVersion = "20.09";
  };

  fonts.fonts = with pkgs; [
    fira-code
    fira-code-symbols
  ];

  time.timeZone = "America/Montreal";
}
