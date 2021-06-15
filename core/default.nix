{ pkgs, ... }:

{
  imports = [
    ./adb.nix
    ./chronyd.nix
    ./docker.nix
    ./nix.nix
    ./openssh.nix
    ./pipewire.nix
    # ./steam.nix
    ./sudo.nix
    ./systemd-resolved.nix
    ./tailscale.nix
    ./zerotier.nix
    ./dendrite-demo-pinecone.nix
  ];

  # boot.kernelPackages = pkgs.linuxPackages_latest;
  # boot.extraModulePackages = [ config.boot.kernelPackages.exfat-nofuse ];

  environment = {
    pathsToLink = [ "/share/zsh" ];
    systemPackages = with pkgs; [ ntfs3g ];
  };

  home-manager.useGlobalPkgs = true;

  home-manager.users.bbigras = { ... }: {
    imports = [
      pkgs.nur.repos.rycee.hmModules.emacs-init
    ];
  };

  i18n.defaultLocale = "fr_CA.UTF-8";

  networking.useDHCP = false;

  services.flatpak.enable = true;

  services.ipfs = {
    enable = false;
    gatewayAddress = "/ip4/127.0.0.1/tcp/8085";
  };

  programs = {
    gnupg.agent = {
      enable = true;
      #   enableSSHSupport = true;
      pinentryFlavor = "gnome3";
    };
    ssh.startAgent = true;
    wireshark.enable = true;
    zsh.enable = true;
  };

  services = {
    fwupd.enable = true; # TODO: check if needed
  };

  users.mutableUsers = false;

  nixpkgs = {
    config.allowUnfree = true;
    localSystem.system = "x86_64-linux";
  };

  fonts.fonts = with pkgs; [
    fira-code
    fira-code-symbols
    meslo-lgs-nf
  ];

  sound.enable = true;
  time.timeZone = "America/Montreal";

  system.stateVersion = "20.09";
}
