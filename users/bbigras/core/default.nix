{ impermanence, nix-index-database, pkgs, ... }: {
  imports = [
    impermanence.nixosModules.home-manager.impermanence
    nix-index-database.hmModules.nix-index

    ./atuin.nix
    ./btop.nix
    ./git.nix
    ./emacs.nix
    ./ssh.nix
    ./tmux.nix
    ./xdg.nix
    ./zsh.nix
  ];

  home = {
    username = "bbigras";
    stateVersion = "22.11";
    packages = with pkgs; [
      kalker # calc

      mosh
      neofetch

      ripgrep

      google-chrome
      remmina

      # media
      pavucontrol

      # games
      # lutris

      # twitch
      # streamlink
      chatterino2

      # dev
      bfg-repo-cleaner
      gitAndTools.git-machete
      git-filter-repo
      gist
      gitAndTools.gh
      colordiff

      # net
      croc
      qbittorrent
      rclone
      tailscale
      tcpdump
      webwormhole
      wireshark
      xh

      # nix
      cachix
      comma
      manix
      nix-update
      nixpkgs-fmt
      nixpkgs-review

      # cool cli tools
      fd
      hexyl
      procs
      pwgen
      sd # find & replace
      bandwhich
      dogdns
      btop

      # Android
      # android-studio
      # scrcpy

      # security?
      bitwarden-cli

      # backup
      restic
      # kopia

      # gist gopass  weechat

      # utils
      file
      strace

      # rust
      cargo
      cargo-audit
      cargo-outdated
      # cargo-asm
      # cargo-bloat
      # cargo-crev
      cargo-expand
      cargo-flamegraph
      # cargo-fuzz
      # cargo-geiger
      cargo-sweep
      cargo-tarpaulin
      cargo-udeps

      # games
      starsector
      mangohud
      heroic

      compsize

      pv

      asciinema # record the terminal
      docker-compose # docker manager
      ncdu # disk space info (a better du)
      prettyping # a nicer ping
      rnix-lsp # nix lsp server

      feh # light-weight image viewer
      killall
      ghidra-bin

      unar
      dbeaver
      josm
      ntp

      # Go
      # go
      # gopls

      # remote
      anydesk
      rustdesk

      # kubernetes
      k9s
      kdash
      kubectl
      kubectx
      kubelogin-oidc
      # istioctl
      kubernetes-helm
      kind

      anytype

      # perf
      sysstat

      ventoy
      docker-credential-helpers
      # freeplane
      viddy
      streamlink
      beets-unstable
      natscli
      joplin-desktop
      just
      megasync
      yt-dlp
      zrok
      aichat
      quickemu
    ];
    shellAliases = {
      cat = "${pkgs.bat}/bin/bat";
      cls = "clear";
      j = "${pkgs.just}/bin/just";
      ".j" = "${pkgs.just}/bin/just --justfile ~/.user.justfile";
      less = ''${pkgs.bat}/bin/bat --paging=always --pager "${pkgs.less}/bin/less -RF"'';
      man = "${pkgs.bat-extras.batman}/bin/batman";
    };
  };

  programs = {
    aria2.enable = true;
    atuin = {
      enable = true;
      settings.auto_sync = true;
    };
    bat.enable = true;
    exa = {
      enable = true;
      enableAliases = true;
    };
    jq.enable = true;
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
      colors = {
        bg = "#1e1e1e";
        "bg+" = "#1e1e1e";
        fg = "#d4d4d4";
        "fg+" = "#d4d4d4";
      };
    };
    gpg.enable = true;
    navi.enable = true;
    nix-index.enable = true;
    sqls.enable = true;

    bashmount.enable = true;

    ssh = {
      enable = true;
      controlMaster = "auto";
      controlPersist = "10m";
      hashKnownHosts = true;

      extraOptionOverrides = {
        AddKeysToAgent = "confirm";
        VerifyHostKeyDNS = "ask";
      };
    };
    tealdeer.enable = true;
    zoxide.enable = true;
    nushell.enable = false;
    zellij.enable = true;
  };

  services = {
    # kdeconnect.enable = true;
    # spotifyd.enable = true;
    megasync.enable = true;
    syncthing.enable = true;
    easyeffects.enable = true;
    pantalaimon = {
      enable = true;
      settings = {
        Default = {
          # LogLevel = "Debug";
          SSL = true;
        };
        local-matrix = {
          Homeserver = "https://matrix.org";
          ListenAddress = "127.0.0.1";
          ListenPort = 8009;
          # UseKeyring = false;
          IgnoreVerification = true;
        };
      };
    };
    systembus-notify.enable = true;
  };

  home.language.base = "fr_CA.UTF-8";

  systemd.user.startServices = "sd-switch";

  xdg.configFile."nixpkgs/config.nix".text = "{ allowUnfree = true; }";
}
