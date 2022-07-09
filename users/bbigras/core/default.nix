{ pkgs, ... }:

let
  my_dwarf_fortress = pkgs.dwarf-fortress-packages.dwarf-fortress-full.override { theme = "vettlingr"; enableIntro = false; };
in
{
  imports = [
    ./atuin.nix
    ./btop.nix
    ./git.nix
    ./emacs.nix
    ./tmux.nix
    ./xdg.nix
    ./zsh.nix
    ./taskwarrior.nix
  ];

  home = {
    stateVersion = "21.05";
    packages = with pkgs; [
      kalker # calc

      mosh
      neofetch

      ripgrep

      google-chrome
      remmina

      # espeak
      socat
      websocat

      # media
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
      bfg-repo-cleaner
      gitAndTools.git-absorb
      gitAndTools.gitui
      gitAndTools.git-machete
      git-filter-repo
      gist
      gitAndTools.gh
      colordiff
      wrangler # cloudflare workers
      tcpdump

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
      hyperspace-cli

      # nix
      cachix
      comma
      nix-prefetch
      nix-prefetch-scripts
      nix-prefetch-github
      nix-review
      nix-update
      nixpkgs-fmt
      nixos-shell
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
      sd # find & replace
      eva # calc
      bandwhich
      dogdns
      btop

      # Android
      # android-studio
      scrcpy

      # security?
      bitwarden-cli

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
      my_dwarf_fortress
      starsector

      compsize

      pv
      rclone

      asciinema # record the terminal
      docker-compose # docker manager
      ncdu # disk space info (a better du)
      prettyping # a nicer ping
      rnix-lsp # nix lsp server

      xdragon
      feh # light-weight image viewer
      killall
      ghidra-bin

      unar
      dbeaver
      obsidian
      logseq
      cloudflared
      tmate
      josm
      ntp

      # Go
      go
      gopls

      # remote
      anydesk
      zoom-us
      teams
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
      pueue

      # perf
      sysstat

      ventoy-bin
      docker-credential-helpers
      freeplane
    ];
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
    just.enable = true;
    fzf.enable = true;
    gpg.enable = true;
    navi.enable = true;
    sqls.enable = true;

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
    systembus-notify.enable = true;
  };

  home.language.base = "fr_CA.UTF-8";

  systemd.user.startServices = "sd-switch";

  xdg.configFile."nixpkgs/config.nix".text = "{ allowUnfree = true; }";
}
