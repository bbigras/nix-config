{ pkgs, ... }: {
  imports = [
    ./btop.nix
    ./git.nix
    ./htop.nix
    ./emacs.nix
    ./tmux.nix
    ./xdg.nix
    ./zsh.nix
  ];

  home = {
    stateVersion = "21.05";
    packages = with pkgs; [
      kalker # calc

      mosh
      neofetch

      ripgrep
      tealdeer

      google-chrome
      remmina

      # espeak
      socat
      websocat

      # media
      handbrake
      vlc
      gimp
      mediainfo
      pavucontrol

      # games
      # lutris

      # twitch
      # streamlink
      chatterino2

      # comm
      discord

      # dev
      gitAndTools.git-absorb
      gitAndTools.gitui
      gitAndTools.git-machete
      git-filter-repo
      gist
      gitAndTools.gh
      colordiff
      wrangler # cloudflare workers
      tcpdump
      radicle-upstream

      # net
      croc
      webwormhole
      qbittorrent
      # youtube-dl
      wireshark
      dnsutils
      oneshot
      tailscale
      ht-rust

      # nix
      cachix
      nix-prefetch
      nix-prefetch-scripts
      nix-prefetch-github
      nix-review
      nix-update
      nixpkgs-fmt
      nixos-shell
      rnix-lsp
      manix

      # cool cli tools
      fd
      hexyl
      zenith
      # dust
      procs
      hyperfine
      pwgen
      rage
      navi # cheat sheet
      sd # find & replace
      eva # calc
      bandwhich
      dogdns
      btop

      vault
      terraform_0_14

      # Android
      # android-studio
      scrcpy

      # security?
      bitwarden-cli

      # pour Laptop
      powerstat

      # backup
      restic
      kopia

      # gist gopass  weechat

      # utils
      file
      tcpdump
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
      # my_dwarf_fortress
      starsector

      compsize

      pv
      rclone

      asciinema # record the terminal
      docker-compose # docker manager
      ncdu # disk space info (a better du)
      prettyping # a nicer ping
      rnix-lsp # nix lsp server

      dragon-drop
      feh # light-weight image viewer
      killall
      ghidra-bin

      unar
      tab-rs
      barrier
      dbeaver
      obsidian
      cloudflared
      tmate
      josm
      ntp

      # Go
      go
      gopls

      # kubernetes
      k9s
      kubectl
      # istioctl
      kubernetes-helm

      nomad
      #hyperspace-cli
      anytype
      pueue

      # perf
      sysstat

    ];
  };

  programs = {
    aria2.enable = true;
    bat.enable = true;
    exa = {
      enable = true;
      enableAliases = true;
    };
    jq.enable = true;
    mcfly.enable = true;
    fzf.enable = true;
    gpg.enable = true;

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

    zoxide.enable = true;
    nushell.enable = true;
    taskwarrior = {
      enable = true;
      colorTheme = "dark-blue-256";
      config = {
        confirmation = false;
        report.minimal.filter = "status:pending";
        report.active.columns = [ "id" "start" "entry.age" "priority" "project" "due" "description" ];
        report.active.labels = [ "ID" "Started" "Age" "Priority" "Project" "Due" "Description" ];
      };
    };
  };

  services = {
    dropbox.enable = true;
    # kdeconnect.enable = true;
    # spotifyd.enable = true;
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
        };
      };
    };
  };

  home.language.base = "fr_CA.UTF-8";

  systemd.user.startServices = "sd-switch";

  xdg.configFile."nixpkgs/config.nix".text = "{ allowUnfree = true; }";
}
