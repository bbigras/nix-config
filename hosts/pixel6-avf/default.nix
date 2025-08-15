# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

# NixOS-WSL specific options are documented on the NixOS-WSL repository:
# https://github.com/nix-community/NixOS-WSL

{
  config,
  lib,
  pkgs,
  catppuccin,
  home-manager,
  hostType,
  minimal-emacs-d,
  nix-index-database,
  nixos-avf,
  nur,
  ...
}:

let
  nurNoPkgs = import nur {
    pkgs = null;
    nurpkgs = pkgs;
  };
in
{
  imports = [
    catppuccin.nixosModules.catppuccin
    home-manager.nixosModules.home-manager
    nix-index-database.nixosModules.nix-index
    nixos-avf.nixosModules.avf
    ../../core/openssh.nix
    # ../../core/podman.nix
    # ../../core/resolved.nix
    ../../core/tailscale.nix
    ../../core/tmux.nix
  ];

  programs.fish.enable = true;
  time.timeZone = "America/Montreal";

  i18n.defaultLocale = "fr_CA.UTF-8";

  environment.systemPackages = with pkgs; [
    ghostty.terminfo
    nix-output-monitor
  ];

  nix.settings = {
    experimental-features = [
      "nix-command"
      "flakes"
    ];
    substituters = [
      "http://192.168.68.6:8501?priority=1"
    ];
    trusted-public-keys = [
      "192.168.68.6:zSAiwQJTX02yGP2NSof1Pin339R5YP+91Y5xdaqFsnU="
    ];
  };

  catppuccin = {
    enable = true;
    flavor = "mocha";
    accent = "blue";
  };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    extraSpecialArgs = {
      inherit
        hostType
        nix-index-database
        nixos-avf
        catppuccin
        minimal-emacs-d
        ;
    };
  };

  avf.defaultUser = "bbigras";

  nix.trustedUsers = [
    "bbigras"
  ];

  users.users.bbigras = {
    shell = lib.mkIf config.programs.fish.enable pkgs.fish;
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGPpDAsQDRslxy69ylheWAtg2synerGqkCeCw6F4ISXp TKey"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIP2Eo0xWZ1VPs5iHlDd3j+O+3I1qx4VqDaXpkL6phB6Z bbigras@desktop"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICaqQycIPmT5lvdqdIQwcy+pitleXZtK0j8RsphADcfa bbigras@laptop"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGZqHDSyyXc5Zs5XzXveQaOzNoSMPLtY686W5/eVISuQ bbigras@pixel6"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCQErMfuhLr14DAHaUSgTLArydzPoLzeOzkYAzf/ye3qvP/vVXeGe4ruWWxvro0yS3DUlfUpmWUU3KRyv3ZN9z6Q9FnDsKKv+GXTrZq4owymd6NWKOLl2F6LGwUGgnkwvit5TDUVolCvIQTT7F/qLgYvmr0q1nTunrl+uPVNXFiAyalhIPMVU+atw/pNmp3JfIqBRefVlrxeoCQ81/nRhQJcNNXjRxSzIeKu80wwCxODYOBtHdIP/NJEzhAMOq/HLabC7ehZtNohweEAlK71HycqwWSNNonEBU0g9R0r/VfXiENa4x+IY5fvMjsdOj53dZuXCDV0AjOmd8sJoepjF7l pubkeygenerator@mobiledevice"
    ];
  };

  home-manager.users.bbigras = {
    # The home.stateVersion option does not have a default and must be set
    home.stateVersion = "20.09";

    imports = [
      catppuccin.homeModules.catppuccin
      nurNoPkgs.repos.rycee.hmModules.emacs-init
      ../../users/bbigras/core/atuin.nix
      ../../users/bbigras/core/emacs
      ../../users/bbigras/core/fish.nix
      ../../users/bbigras/core/git.nix
      ../../users/bbigras/core/jjui.nix
      ../../users/bbigras/core/jujutsu.nix
      ../../users/bbigras/core/tmux.nix
      ../../users/bbigras/core/yazi.nix
      ../../users/bbigras/core/zsh.nix
    ];

    catppuccin = {
      enable = true;
      flavor = "mocha";
      accent = "blue";
    };

    programs = {
      bat = {
        enable = true;
      };
      carapace.enable = true;
      command-not-found.enable = true;
      direnv = {
        enable = true;
        silent = true;
        nix-direnv.enable = true;
      };
      emacs = {
        enable = true;
        package = lib.mkForce pkgs.emacs-nox;
        init.usePackage = {
          god-mode.enable = true;
        };
      };
      eza.enable = true;
      fzf = {
        enable = true;
        tmux.enableShellIntegration = true;
        tmux.shellIntegrationOptions = [ "-d 40%" ];
        defaultCommand = "fd --type f";
        defaultOptions = [
          "--height 40%"
          "--border"
        ];
        changeDirWidgetCommand = "fd --type d"; # alt+c
        changeDirWidgetOptions = [
          "--preview 'tree -C {} | head -200'"
        ];
        fileWidgetCommand = "fd --type f";
        fileWidgetOptions = [
          "--preview 'head {}'"
        ];
      };
      jq.enable = true;
      htop.enable = true;
      ripgrep.enable = true;
      pay-respects.enable = true;
      ssh = {
        enable = true;
        controlMaster = "auto";
        controlPersist = "10m";
        hashKnownHosts = true;
        includes = [
          "~/.ssh/devpod"
        ];
        extraOptionOverrides = {
          AddKeysToAgent = "confirm";
          VerifyHostKeyDNS = "ask";
        };
      };
      starship.enable = true;
      tealdeer.enable = true;
      zoxide.enable = true;
    };

    home.packages = with pkgs; [
      (neofetch.override { x11Support = false; })
      attic-client
      cachix
      croc
      doggo
      dumbpipe
      faketty
      fd
      git-annex
      git-remote-gcrypt
      incus.client
      just
      k9s
      kubectl
      kubectx
      kubelogin-oidc
      kubernetes
      kubernetes-helm
      kubie
      libqalculate
      mosh
      nix-output-monitor
      oneshot
      openssh
      prettyping
      pwgen
      restic
      sendme
      spacer
      talosctl
      viddy
      xh
      zrok
    ];

    services = {
      syncthing.enable = true;
    };

    systemd.user.services = {
      syncthing = {
        Service = {
          StandardOutput = "null";
        };
      };
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.05"; # Did you read the comment?
}
