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
    nix-index-database.homeModules.nix-index
    catppuccin.homeModules.catppuccin

    ./asciinema.nix
    ./atuin.nix
    ./btop.nix
    ./easyeffects.nix
    ./git.nix
    ./jjui.nix
    ./jujutsu.nix
    ./lutris.nix
    ./emacs
    ./process-compose.nix
    ./tmux.nix
    ./xdg.nix
    ./fish.nix
    ./vicinae.nix
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
      resticprofile
      tkey-ssh-agent
      jan
      sops
      mergiraf
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
    keepassxc = {
      enable = true;
      settings = {
        Browser.Enabled = false;
        # Browser.UpdateBinaryPath = false;
        # MinimizeAfterUnlock = true;
        FdoSecrets.Enabled = true;
        GUI = {
          AdvancedSettings = true;
          ApplicationTheme = "dark";
          CompactMode = true;
          HidePasswords = true;
          LaunchAtStartup = true;
        };
        SSHAgent.Enabled = false;
      };
    };
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
    gcc = {
      enable = true;
      colors = {
        error = "01;31";
      };
    };
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
    nix-search-tv = {
      enable = true;
      settings = {
        indexes = [
          "nixpkgs"
          "home-manager"
          "nixos"
        ];
      };
    };
    nix-your-shell.enable = true;
    pay-respects.enable = true;
    radicle = {
      enable = true;
      settings = {
        node = {
          listen = [ "0.0.0.0:8776" ];
        };
      };
    };
    ripgrep.enable = true;
    sesh.enable = true;
    sqls.enable = true;
    ssh = {
      enable = true;
      enableDefaultConfig = false;
      matchBlocks."*" = {
        controlMaster = "auto";
        controlPersist = "10m";
        hashKnownHosts = true;
        addKeysToAgent = "confirm";
      };
    };
    starship.enable = true;
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
    swappy = {
      enable = true;
      settings = {
        Default = {
          auto_save = false;
          custom_color = "rgba(193,125,17,1)";
          early_exit = false;
          fill_shape = false;
          line_size = 5;
          paint_mode = "brush";
          save_dir = "$HOME/Desktop";
          save_filename_format = "swappy-%Y%m%d-%H%M%S.png";
          show_panel = false;
          text_font = "sans-serif";
          text_size = 20;
          transparency = 50;
          transparent = false;
        };
      };
    };
    television = {
      enable = true;
      settings = {
        tick_rate = 50;
        ui = {
          use_nerd_font_icons = true;
        };
      };
    };
    trippy.enable = true;
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
    vivid = {
      enable = true;
      activeTheme = "catppuccin-mocha";
      themes = {
        catppuccin-mocha = builtins.fetchurl {
          url = "https://raw.githubusercontent.com/sharkdp/Vivid/refs/heads/master/themes/catppuccin-mocha.yml";
          sha256 = "sha256:1hfwaf8lfq32w9vcdlbwrq5hwwz725i7icavg6qs66awpzqqb34k";
        };
      };
    };
    tealdeer.enable = true;
    zoxide.enable = true;
  };

  services = {
    # spotifyd.enable = true;
    radicle.node = {
      enable = true;
      lazy.enable = true;
    };
    syncthing.enable = true;
    tailscale-systray.enable = true;
    psd = {
      enable = true;
      browsers = [
        "firefox"
      ];
    };
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
