{ hostType, impermanence, nix-index-database, pkgs, catppuccin, lib, ... }: {
  imports = [
    impermanence.nixosModules.home-manager.impermanence
    nix-index-database.hmModules.nix-index
    catppuccin.homeManagerModules.catppuccin

    ./atuin.nix
    ./btop.nix
    ./git.nix
    ./emacs
    ./ssh.nix
    ./tmux.nix
    ./xdg.nix
    ./zsh.nix
  ];

  catppuccin.flavour = "mocha";

  # XXX: Manually enabled in the graphic module
  dconf.enable = false;

  home = {
    username = "bbigras";
    stateVersion = "22.11";
    packages = with pkgs; [
      nix-closure-size
      truecolor-check

      libqalculate

      mosh

      ripgrep

      # dev
      bfg-repo-cleaner
      gitAndTools.git-machete
      git-filter-repo
      git-ps-rs
      gist
      gitAndTools.gh
      colordiff

      # net
      croc

      rclone
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
      procs
      pwgen
      sd # find & replace
      bandwhich
      doggo
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


      compsize

      pv

      # asciinema # record the terminal
      docker-compose # docker manager
      ncdu # disk space info (a better du)
      prettyping # a nicer ping

      feh # light-weight image viewer
      killall

      unar
      ntp

      # Go
      # go
      # gopls

      # kubernetes
      k9s
      kdash
      kubectl
      kubectx
      kubelogin-oidc
      # istioctl
      kubernetes-helm
      kind

      # perf
      sysstat

      ventoy
      docker-credential-helpers
      viddy
      natscli
      just

      yt-dlp
      zrok
      aichat
      quickemu
      mediainfo

      git-annex
      git-remote-gcrypt
      qalculate-gtk
      devpod
      open-interpreter
      altair
      broot
      usbimager
    ];
    shellAliases = {
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
    bat = {
      enable = true;
      extraPackages = with pkgs.bat-extras; [ batman ];
      catppuccin.enable = true;
    };
    carapace.enable = true;
    eza.enable = true;
    jq.enable = true;
    fzf = {
      enable = true;
      tmux.enableShellIntegration = true;
      defaultCommand = "fd --type f";
      changeDirWidgetCommand = "fd --type d"; # alt+c
      fileWidgetCommand = "fd --type f";
      catppuccin.enable = true;
    };
    gpg.enable = true;
    navi.enable = true;
    nix-index.enable = true;
    sqls.enable = true;

    bashmount.enable = true;

    rio.enable = true;
    sapling = {
      enable = true;
      userEmail = "bigras.bruno@gmail.com";
      userName = "Bruno Bigras";
    };
    ssh = {
      enable = true;
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
    # spotifyd.enable = true;
    syncthing.enable = true;
    systembus-notify.enable = true;
  };

  home.language.base = "fr_CA.UTF-8";

  systemd.user.startServices = "sd-switch";

  xdg.configFile."nixpkgs/config.nix".text = "{ allowUnfree = true; }";
}
