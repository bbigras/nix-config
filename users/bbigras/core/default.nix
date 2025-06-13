{
  config,
  hostType,
  impermanence,
  nix-index-database,
  pkgs,
  lib,
  catppuccin,
  ...
}:

{
  imports = [
    impermanence.nixosModules.home-manager.impermanence
    nix-index-database.hmModules.nix-index
    catppuccin.homeModules.catppuccin

    ./atuin.nix
    ./btop.nix
    ./easyeffects.nix
    ./git.nix
    ./jjui.nix
    ./jujutsu.nix
    ./emacs
    ./process-compose.nix
    ./tmux.nix
    ./xdg.nix
    ./fish.nix
    ./yazi.nix
  ];

  catppuccin = {
    enable = true;
    flavor = "mocha";
    accent = "blue";
  };

  home = {
    username = "bbigras";
    stateVersion = "24.05";
    packages = with pkgs; [
      nix-closure-size
      mosh

      # dev
      colordiff

      # net
      dumbpipe
      croc
      rclone
      sendme
      tailscale
      tcpdump
      webwormhole
      xh

      # nix
      cachix
      comma
      manix
      nix-update
      nixpkgs-fmt

      # cool cli tools
      fd
      hexyl
      pwgen
      sd # find & replace
      doggo

      # Android
      # android-studio
      # scrcpy

      # backup
      restic

      # utils
      file
      strace

      # rust
      cargo

      pv

      docker-compose
      prettyping # a nicer ping

      killall

      unar

      # kubernetes
      kubectl
      kubectx
      kubelogin-oidc
      # istioctl
      kubernetes-helm
      kind

      docker-credential-helpers
      viddy
      natscli
      just

      # usb imagers
      usbimager

      # media
      mediainfo
      spotube
      kooha

      zrok

      devpod
      altair
      broot
      distrobox
      incus.client
      srgn
      attic-client
      aider-chat
      git-annex
      git-remote-gcrypt
      spacer
      faketty
      television
      radicle-node
      resticprofile
      tkey-ssh-agent
      jan
      anki
      sops
    ];
    shellAliases = {
      cls = "clear";
      j = "${pkgs.just}/bin/just";
      ".j" = "${pkgs.just}/bin/just --justfile ~/.user.justfile";
      less = "${pkgs.bat}/bin/bat";
    };
  };

  programs = {
    atuin = {
      enable = true;
      settings.auto_sync = true;
    };
    bat = {
      enable = true;
    };
    carapace.enable = true;
    eza.enable = true;
    jq.enable = true;
    kubecolor = {
      enable = true;
      enableAlias = true;
      settings = {
        preset = "dark";
        paging = "auto";
      };
    };
    fzf = {
      enable = true;
      tmux.enableShellIntegration = true;
      defaultCommand = "fd --type f";
      changeDirWidgetCommand = "fd --type d"; # alt+c
      fileWidgetCommand = "fd --type f";
    };
    gpg.enable = true;
    nix-index.enable = true;
    nix-your-shell.enable = true;
    pay-respects.enable = true;
    ripgrep.enable = true;
    sesh.enable = true;
    sqls.enable = true;
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
    streamlink = {
      enable = true;
      settings = {
        player = "${pkgs.mpv}/bin/mpv";
        player-args = "--cache 2048";
        player-no-close = true;
        twitch-disable-ads = true;
        twitch-low-latency = true;
      };
    };
    vesktop = {
      enable = true;
      settings = {
        appBadge = false;
        arRPC = true;
        checkUpdates = false;
        customTitleBar = false;
        disableMinSize = true;
        minimizeToTray = false;
        tray = false;
        splashBackground = "#000000";
        splashColor = "#ffffff";
        splashTheming = true;
        staticTitle = true;
        hardwareAcceleration = true;
        discordBranch = "stable";
      };
      vencord.settings = {
        autoUpdate = false;
        autoUpdateNotification = false;
        notifyAboutUpdates = false;
        useQuickCss = true;
        disableMinSize = true;
        plugins = {
          # MessageLogger = {
          #   enabled = true;
          #   ignoreSelf = true;
          # };
          FakeNitro.enabled = true;
        };
      };
    };
    tealdeer.enable = true;
    zoxide.enable = true;
  };

  services = {
    # spotifyd.enable = true;
    syncthing.enable = true;
    systembus-notify.enable = true;
  };

  systemd.user.services = {
    syncthing = {
      Service = {
        StandardOutput = "null";
      };
    };
  };

  home.language.base = "fr_CA.UTF-8";

  systemd.user.startServices = "sd-switch";

  xdg.configFile."nixpkgs/config.nix".text = "{ allowUnfree = true; }";
}
