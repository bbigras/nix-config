{ pkgs, ... }:
let
  dummyConfig = pkgs.writeText "configuration.nix" ''
    assert builtins.trace "This is a dummy config, use nixus!" false;
    {}
  '';
  configFile = pkgs.writeText "chrony.conf" ''
    server time.cloudflare.com iburst nts

    initstepslew 1000 time.cloudflare.com

    driftfile /var/lib/chrony/chrony.drift
    keyfile /var/lib/chrony/chrony.keys

    ntsdumpdir /var/lib/chrony/nts
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

  nix.package = pkgs.nixFlakes;
  nix.extraOptions = ''
    experimental-features = nix-command flakes
    keep-outputs = true
    keep-derivations = true
  '';

  # ---
  users.groups.chrony.gid = 61;

  users.users.chrony =
    {
      uid = 61;
      group = "chrony";
      description = "chrony daemon user";
      home = "/var/lib/chrony";
    };

  services.timesyncd.enable = false;

  systemd.services.systemd-timedated.environment = { SYSTEMD_TIMEDATED_NTP_SERVICES = "chronyd.service"; };

  systemd.tmpfiles.rules = [
    "d /var/lib/chrony 0755 chrony chrony - -"
    "f /var/lib/chrony/chrony.keys 0640 chrony chrony -"
  ];

  systemd.services.chronyd =
    {
      description = "chrony NTP daemon";

      wantedBy = [ "multi-user.target" ];
      wants = [ "time-sync.target" ];
      before = [ "time-sync.target" ];
      after = [ "network.target" ];
      conflicts = [ "ntpd.service" "systemd-timesyncd.service" ];

      path = [ pkgs.chrony ];

      unitConfig.ConditionCapability = "CAP_SYS_TIME";
      serviceConfig =
        {
          Type = "simple";
          ExecStart = "${pkgs.chrony}/bin/chronyd -n -m -u chrony -f ${configFile}";

          ProtectHome = "yes";
          ProtectSystem = "full";
          PrivateTmp = "yes";
        };

    };
  # ---

  # boot.kernelPackages = pkgs.linuxPackages_latest;
  # boot.extraModulePackages = [ config.boot.kernelPackages.exfat-nofuse ];
  environment.etc."nixos/configuration.nix".source = dummyConfig;
  environment.systemPackages = with pkgs; [ ntfs3g chrony ];
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
      (import (import ../nix/sources.nix).nixpkgs-cdda-mods)
      (import ../overlays/mkSecret.nix)
    ];
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
