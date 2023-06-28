{ hostType, impermanence, nix-index-database, pkgs, stylix, ... }:

{
  imports = [
    impermanence.nixosModules.home-manager.impermanence
    nix-index-database.hmModules.nix-index
    stylix.homeManagerModules.stylix

    ./atuin.nix
    ./btop.nix
    ./git.nix
    ./emacs.nix
    ./ssh.nix
    # ./email.nix
    ./tmux.nix
    ./xdg.nix
    ./zsh.nix
  ];

  # XXX: Manually enabled in the graphic module
  dconf.enable = false;

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

      # comm
      discord

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
      #mangohud
      #heroic

      compsize

      pv

      # asciinema # record the terminal
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

      # anytype

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
      xournalpp
      mediainfo
      git-annex
      git-remote-gcrypt
      space-station-14-launcher
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
      enable = false;
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

  # https://github.com/Digitalone1/EasyEffects-Presets
  # xdg.configFile."easyeffects/output/LoudnessEqualizer.json".source = "${easyeffects-presets_repo}/LoudnessEqualizer.json";

  # https://github.com/JackHack96/EasyEffects-Presets
  #xdg.configFile."easyeffects/output/Perfect EQ.json".source = "${perfect_eq_repo}/Perfect EQ.json";
  #xdg.configFile."easyeffects/output/Bass Enhancing + Perfect EQ.json".source = "${perfect_eq_repo}/Bass Enhancing + Perfect EQ.json";
  #xdg.configFile."easyeffects/output/Boosted.json".source = "${perfect_eq_repo}/Boosted.json";
  #xdg.configFile."easyeffects/output/Advanced Auto Gain.json".source = "${perfect_eq_repo}/Advanced Auto Gain.json";

  # xdg.configFile."easyeffects/input/Improved Microphone (Male voices, with Noise Reduction).json".source = "${mic_gist_repo}/Improved Microphone (Male voices, with Noise Reduction).json";

  #xdg.configFile."pipewire/filter-chain.conf.d/sink-virtual-surround-5.1-kemar2.conf".source = "${pipewire_repo}/src/daemon/filter-chain/sink-virtual-surround-5.1-kemar.conf";

  # /home/bbigras/src/virtual-surround/resources/hrir_kemar/hrir-kemar.wav

  # /nix/store/dr1vhsvi39i3nzi3ak4ncrcabqf59m87-pipewire-c6ffeee/src/daemon/filter-chain/sink-virtual-surround-5.1-kemar.conf
  # /nix/store/nb3v25xs49b1aasjqlpywvv58z1iaqyy-pipewire-0.3.56-lib/share/pipewire/filter-chain/sink-virtual-surround-5.1-kemar.conf
  # /nix/store/jh21g751nxpzsyrrw92hryz8p6a0dwaw-pipewire-0.3.56-lib/share/pipewire/filter-chain/sink-virtual-surround-5.1-kemar.conf
  # /nix/store/vs9fqz47mgy9pi4d5znz7c8gg962vrs3-pipewire-0.3.56-lib/share/pipewire/filter-chain/sink-virtual-surround-5.1-kemar.conf

  home.language.base = "fr_CA.UTF-8";

  stylix = {
    base16Scheme = "${pkgs.base16-schemes}/share/themes/ayu-dark.yaml";
    # XXX: We fetchurl from the repo because flakes don't support git-lfs assets
    #image = pkgs.fetchurl {
    #  url = "https://media.githubusercontent.com/media/bbigras/nix-config/5fcd6f221eb5ea008aa11f9242862f745496ff61/users/bbigras/assets/walls/cool-subaru-of-re-zero-hd-blpy4l9qvbrutnhq.jpg";
    #  hash = "sha256-d9IvPHLs+uh73m6pgdDl//QGwXYrFRZNee1O1EYBw4g=";
    #};
    targets.gnome.enable = hostType == "nixos";
    targets.gtk.enable = hostType == "nixos";
  };

  systemd.user.startServices = "sd-switch";

  xdg.configFile."nixpkgs/config.nix".text = "{ allowUnfree = true; }";
}
