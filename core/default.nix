{ config, pkgs, ... }:
let
  dummyConfig = pkgs.writeText "configuration.nix" ''
    assert builtins.trace "This is a dummy config, use deploy-rs!" false;
    { }
  '';
in
{
  imports = [
    ./aspell.nix
    ./nix.nix
    ./openssh.nix
    ./resolved.nix
    ./tmux.nix
    ./xdg.nix
    ./yggdrasil.nix
    ./zsh.nix
    ./solo2.nix
    ./dendrite-demo-pinecone.nix
  ];

  boot.kernelParams = [ "log_buf_len=10M" ];

  environment = {
    etc."nixos/configuration.nix".source = dummyConfig;
    systemPackages = with pkgs; [
      ntfs3g
      foot.terminfo
      btop
      minikube
      docker-machine-kvm2
    ];
  };

  home-manager = {
    useGlobalPkgs = true;
    verbose = true;
  };

  i18n.defaultLocale = "fr_CA.UTF-8";

  networking = {
    firewall = {
      trustedInterfaces = [ "tailscale0" ];
      allowedUDPPorts = [ config.services.tailscale.port ];
    };
    useDHCP = false;
    useNetworkd = true;
    wireguard.enable = true;
  };

  nix.nixPath = [
    "nixos-config=${dummyConfig}"
    "nixpkgs=/run/current-system/nixpkgs"
    "nixpkgs-overlays=/run/current-system/overlays"
  ];

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.permittedInsecurePackages = [ "electron-13.6.9" ];

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  services.tailscale.enable = true;
  systemd.services.tailscaled.after = [ "network-online.target" "systemd-resolved.service" ];

  system = {
    extraSystemBuilderCmds = ''
      ln -sv ${pkgs.path} $out/nixpkgs
      ln -sv ${../nix/overlays} $out/overlays
    '';

    stateVersion = "21.05";
  };

  systemd.enableUnifiedCgroupHierarchy = true;

  time.timeZone = "America/Montreal";

  services.flatpak.enable = true;

  fonts.fonts = with pkgs; [
    fira-code
    fira-code-symbols
    meslo-lgs-nf
  ];


  users.mutableUsers = true;
}
