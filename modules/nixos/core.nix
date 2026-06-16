{
  config,
  flake,
  lib,
  pkgs,
  ...
}:
let
  inherit (flake) self;
in
{
  imports = with self.nixosModules; [
    aspell
    common
    nix
    nixpkgs
    resolved
    services-veilid
    tmux
  ];

  environment.systemPackages = with pkgs; [
    man-pages
    ghostty.terminfo
  ];

  boot.kernelParams = [ "log_buf_len=10M" ];
  boot.loader.timeout = 0;

  i18n.defaultLocale = "fr_CA.UTF-8";

  networking = {
    firewall = {
      trustedInterfaces = [ "tailscale0" ];
      allowedUDPPorts = [ ];
    };
    useDHCP = false;
    useNetworkd = true;
    wireguard.enable = true;
  };

  security = {
    # pam.services.sudo.u2fAuth = true;
    polkit.enable = true;
    sudo-rs = {
      enable = true;
      wheelNeedsPassword = true;
    };
  };

  services = {
    dbus.implementation = "broker";
    openssh = {
      enable = true;
      settings.PermitRootLogin = lib.mkDefault "no";
    };
    smartd = {
      enable = true;
      notifications = {
        systembus-notify.enable = true;
      };
    };
    timesyncd.enable = false;
    ntpd-rs = {
      enable = true;
      settings = {
        observability.log-level = "warn";
        source = [
          {
            mode = "nts";
            address = "time.cloudflare.com";
          }
          {
            mode = "nts";
            address = "time1.mbix.ca";
          }
          {
            mode = "nts";
            address = "time.web-clock.ca";
          }
        ];
      };
    };
    tailscale = {
      enable = true;
      openFirewall = true;
      useRoutingFeatures = "client";
      disableUpstreamLogging = true;
    };
    fwupd.daemonSettings.EspLocation = config.boot.loader.efi.efiSysMountPoint;
  };

  system = {
    stateVersion = lib.mkDefault "26.05";
  };

  systemd = {
    network.wait-online.anyInterface = true;
  };

  time.timeZone = "America/Montreal";

  users.mutableUsers = false;
}
