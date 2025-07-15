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
      # aider-chat
      git-annex
      git-remote-gcrypt
      spacer
      faketty
      radicle-node
      resticprofile
      tkey-ssh-agent
      jan
      anki
      sops
    ];
    shellAliases = {
      cat = "bat";
      cls = "clear";
      j = "${pkgs.just}/bin/just";
      ".j" = "${pkgs.just}/bin/just --justfile ~/.user.justfile";
      less = "${pkgs.bat}/bin/bat";
      l = "ls";
      la = "ls --all";
      ls = "eza --binary --header --long";
      man = "batman";
    };
  };

  programs = {
    keepassxc.enable = true;
    atuin = {
      enable = true;
      settings.auto_sync = true;
    };
    bat = {
      enable = true;
      extraPackages = with pkgs.bat-extras; [ batman ];
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
    kubeswitch = {
      enable = true;
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
    television = {
      enable = true;
      channels = {
        my-custom = {
          cable_channel = [
            {
              name = "git-log";
              source_command = "git log --oneline --date=short --pretty=\"format:%h %s %an %cd\" \"$@\"";
              preview_command = "git show -p --stat --pretty=fuller --color=always {0}";
            }
            {
              name = "git-log";
              source_command = "git log --oneline --date=short --pretty=\"format:%h %s %an %cd\" \"$@\"";
              preview_command = "git show -p --stat --pretty=fuller --color=always {0}";
            }
          ];
        };
      };
      settings = {
        tick_rate = 50;
        ui = {
          use_nerd_font_icons = true;
          ui_scale = 120;
          show_preview_panel = false;
        };
        keybindings = {
          quit = [
            "esc"
            "ctrl-c"
          ];
        };
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
